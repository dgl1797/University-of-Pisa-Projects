import os, sys, optuna
from torchvision.transforms import Compose, ToTensor, Resize
from dataclasses import dataclass
from torch.nn import Module as TorchModule, CrossEntropyLoss
from torch import optim, no_grad, load as torch_load, Tensor
from torch.utils.data import DataLoader
from tqdm import tqdm
from copy import deepcopy
from torch.cuda import max_memory_allocated

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")))
from src.models import ClassifierModel
from src.utils.ConfigTypes import FinetuneInfo
from src.utils.addons import DynamicLogger, EarlyStopping
from src.utils.Interfaces import EvaluationBase
from src.datasets.loaders import load_classification_fsets
from src.implementations.unlearn.retrain import prepare_evaluate as unlearn_retrain_prepare_evaluation
from src.metrics import UnlearnEval, MIAEval

@dataclass
class TrainState():
    trainmodule: TorchModule = None
    trainloader: DataLoader = None
    validloader: DataLoader = None
    optimizer: optim.Optimizer = None
    monitor: EarlyStopping = None
    evalmetrics: UnlearnEval = None
    nepochs: int = 0
    device: str = "cpu"

@dataclass
class EvalState():
    evalmodule: ClassifierModel = None
    golden: ClassifierModel = None
    testloader: DataLoader = None
    umetrics: UnlearnEval = None
    miametrics: MIAEval = None
    device: str = "cpu"

@dataclass
class HPOState(): pass

# preparation functions #
def prepare_train(exp: FinetuneInfo, dr: str, sn: str) -> TrainState:
    trainstate = TrainState(); dsdir = f"{exp.dataset.name.upper()}_Data"
    split_loc = os.path.join(dr, dsdir, "image_splits", sn)
    unseen_file = os.path.join(dr, dsdir, "class_splits", f"{exp.method.origin}.txt")
    forget_file = os.path.join(dr, dsdir, "class_splits", f"{exp.method.forget}.txt")
    transform = Compose([Resize((exp.classifier.input_size, exp.classifier.input_size)), ToTensor()])
    trainds, _, validds = load_classification_fsets(split_loc, unseen_file, forget_file, transform)
    origin_checkpoint = f"{exp.classifier.name}_{exp.dataset.name}.pt"
    assert trainds.nclasses() == exp.dataset.ntotal_class - exp.dataset.nunseen_test, f"bad split: train dataset is missing some seen classes"
    assert validds.nclasses() == exp.dataset.ntotal_class - exp.dataset.nunseen_test, f"bad split: valid dataset is missing some seen classes"

    # training state composition
    trainstate.trainmodule = ClassifierModel(exp.classifier, trainds.nclasses(), finetune=True)
    trainstate.trainmodule.load_state_dict(torch_load(os.path.join("out", "checkpoints", sn, "origin", exp.method.origin, origin_checkpoint)))
    trainstate.optimizer = getattr(optim, exp.method.hyperparameters.optimizer.name)(trainstate.trainmodule.parameters(), **exp.method.hyperparameters.optimizer.params)
    trainstate.trainloader = DataLoader(trainds, exp.method.hyperparameters.batch_size, True, num_workers=exp.nworkers, persistent_workers=True)
    
    # validation state composition
    trainstate.validloader = DataLoader(validds.on_full(), exp.method.hyperparameters.batch_size, False, num_workers=exp.nworkers, persistent_workers=True)
    trainstate.evalmetrics = UnlearnEval(trainds.nclasses(), exp.device, trainds.translate_labels(trainds.forget))
    trainstate.monitor = EarlyStopping(trainstate.trainmodule, exp.method.hyperparameters.patience, minimize=False)
    
    # extras
    trainstate.nepochs = exp.method.hyperparameters.max_epochs
    trainstate.device = exp.device
    return trainstate

def prepare_evaluate(exp: FinetuneInfo, in_module: TorchModule, dr: str, sn: str, replace: bool) -> EvalState:
    evalstate: EvalState = unlearn_retrain_prepare_evaluation(exp, in_module, dr, sn, replace)
    goldenloc = os.path.join("out", "checkpoints", sn, "unlearn", f"retrain_{exp.method.origin}", f"{exp.method.forget}_{exp.classifier.name}_{exp.dataset.name}.pt")
    evalstate.golden = ClassifierModel(exp.classifier, evalstate.testloader.dataset.nclasses(), finetune=False)
    evalstate.golden.load_state_dict(torch_load(goldenloc))
    return evalstate

def prepare_hpo(exp: FinetuneInfo, dr: str, sn: str, study_name: str, storage_loc: str, seed: int) -> optuna.Study:
    already_exists = False
    if os.path.exists(storage_loc): already_exists = True
    study = optuna.create_study(study_name=study_name, storage=f"sqlite:///{storage_loc}", load_if_exists=True)
    if already_exists: return hpo_analysis(study)
    study.optimize(lambda trial: objective(HPOState(), trial), n_trials=exp.hpo.ntrials)
    return study

# core functions #
@no_grad()
def validate(state: TrainState, epoch: int, strict: bool = True):
    state.trainmodule.eval()
    state.evalmetrics.zero()
    for x,y in tqdm(state.validloader, desc=f"Validating {epoch+1}/{state.nepochs}"):
        x,y = x.to(state.device), y.to(state.device)
        yp = state.trainmodule(x)
        state.evalmetrics(yp, y)
    return state.monitor.new_metric(
        state.evalmetrics.accuracy(retain=True)-state.evalmetrics.accuracy(retain=False), silent=False, strict=strict
    )

def train(state: TrainState, logger: DynamicLogger) -> TorchModule:
    criterion = CrossEntropyLoss()
    state.trainmodule.to(state.device)
    for epoch in range(state.nepochs):
        state.trainmodule.train()
        for x,y in tqdm(state.trainloader, desc=f"Unlearn {epoch+1}/{state.nepochs}"):
            state.optimizer.zero_grad()
            x,y = x.to(state.device), y.to(state.device)
            yp = state.trainmodule(x)
            loss = criterion(yp, y)
            loss.backward()
            state.optimizer.step()

        stop_condition = validate(state, epoch)
        if logger is not None: logger.new_metrics(["Unlearn_Baselines_Training"], [{f"Finetune/{k}": v[0] for k,v in state.evalmetrics.todict(train=True).items()}], log_step=True)
        if stop_condition: break
    
    state.trainmodule.load_state_dict(deepcopy(state.monitor.best_state))
    return state.trainmodule.to("cpu")

@no_grad()
def evaluate(state: EvalState, logger: DynamicLogger) -> EvaluationBase:
    if not state.umetrics.loaded:
        state.evalmodule.to(state.device).eval()
        state.golden.to(state.device).eval()
        for x,y in tqdm(state.testloader, desc="Evaluation"):
            x: Tensor = x.to(state.device); y: Tensor = y.to(state.device)
            yu = state.evalmodule(x)
            ye = state.golden(x)
            state.umetrics(yu, y)
            state.umetrics.distributions_deltas(ye, yu)

    state.evalmodule.to("cpu"); state.golden.to("cpu")
    if logger is not None: 
        resultdict = state.umetrics.todict() | state.miametrics.todict(nfolds=10)
        logger.new_metrics(["Unlearn_Baselines"], [{f"Finetune/{k}": v[0] for k,v in resultdict.items()}])
        print("Metrics:\n", {k: f"{v[0]*100:.2f}%" if v[1] else f"{v[0]:.4f}" for k,v in resultdict.items()})

    return [state.umetrics, state.miametrics]

def objective(state: HPOState, trial: optuna.Trial): pass
def hpo_analysis(study: optuna.Study)->optuna.Study: return study