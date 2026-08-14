import os, sys, optuna
from dataclasses import dataclass
from torch.nn import Module as TorchModule, CrossEntropyLoss
from torch import optim, Tensor, no_grad
from torch.utils.data import DataLoader
from tqdm import tqdm
from torchvision.transforms import Compose, Resize, ToTensor
from copy import deepcopy

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.utils.ConfigTypes import OriginInfo
from src.utils.addons import DynamicLogger
from src.datasets.loaders import load_classification_sets
from src.models import ClassifierModel
from src.utils.addons import EarlyStopping
from src.utils.Interfaces import EvaluationBase
from src.metrics import ClassificationEval, MIAEval

@dataclass
class TrainState():
    trainmodule: TorchModule = None
    optimizer: optim.Optimizer = None
    trainloader: DataLoader = None
    validloader: DataLoader = None
    monitor: EarlyStopping = None
    criterion: TorchModule = None
    nepochs: int = 0
    device: str = "cpu"

@dataclass
class EvalState():
    evalmodule: TorchModule = None
    testloader: DataLoader = None
    metrics: EvaluationBase = None
    miametrics: MIAEval = None
    device: str = "cpu"

@dataclass
class HPOState(): pass

# preparation functions #
def prepare_train(exp: OriginInfo, dr: str, sn: str) -> TrainState:
    train_state = TrainState(); dsdir = f"{exp.dataset.name.upper()}_Data"
    split_loc = os.path.join(dr, dsdir, "image_splits", sn)
    unseen_file = os.path.join(dr, dsdir, "class_splits", f"{exp.unseen}.txt") if exp.unseen != "none" else None
    transform = Compose([Resize((exp.classifier.input_size, exp.classifier.input_size)), ToTensor()])
    trainds, _, validds = load_classification_sets(split_loc, unseen_file, transform)
    assert trainds.nclasses() == exp.dataset.ntotal_class - exp.dataset.nunseen_test, f"bad split: train dataset is missing some seen classes"
    assert validds.nclasses() == exp.dataset.ntotal_class - exp.dataset.nunseen_test, f"bad split: valid dataset is missing some seen classes"
    
    # state composition
    train_state.nepochs = exp.hyperparameters.max_epochs
    train_state.device = exp.device
    train_state.trainmodule = ClassifierModel(exp.classifier, trainds.nclasses(), finetune=True)
    # trainstate.trainmodule.parameters() is a yielder, this means that it will yield a parameter from the trainmodule reference
    # as it is requested by an iterator. This means that moving trainmodule to cuda is safe as the yielding will be done on the
    # moved parameters hence computing it on cuda
    train_state.optimizer = getattr(optim, exp.hyperparameters.optimizer.name)(train_state.trainmodule.parameters(), **exp.hyperparameters.optimizer.params)
    train_state.trainloader = DataLoader(trainds, exp.hyperparameters.batch_size, True, num_workers=exp.nworkers, persistent_workers=True)
    train_state.validloader = DataLoader(validds, exp.hyperparameters.batch_size, False, num_workers=exp.nworkers, persistent_workers=True)
    train_state.monitor = EarlyStopping(train_state.trainmodule, exp.hyperparameters.patience)
    train_state.criterion = CrossEntropyLoss()
    return train_state

def prepare_evaluate(exp: OriginInfo, in_module: TorchModule, dr: str, sn: str, replace: bool) -> EvalState:
    eval_state = EvalState(); dsdir = f"{exp.dataset.name.upper()}_Data"
    split_loc = os.path.join(dr, dsdir, "image_splits", sn)
    unseen_file = os.path.join(dr, dsdir, "class_splits", f"{exp.unseen}.txt")
    transform = Compose([Resize((exp.classifier.input_size, exp.classifier.input_size)), ToTensor()])
    trainds, testds, _ = load_classification_sets(split_loc, unseen_file, transform)
    assert testds.nclasses() == exp.dataset.ntotal_class - exp.dataset.nunseen_test, f"bad split: test dataset is missing some seen classes"

    # state composition
    metric_location = os.path.join("out", "evaluations", sn, "origin", exp.unseen, f"{exp.classifier.name}_{exp.dataset.name}")
    eval_state.evalmodule = in_module
    eval_state.testloader = DataLoader(testds, exp.hyperparameters.batch_size, False, num_workers=exp.nworkers, persistent_workers=True)
    eval_state.metrics = ClassificationEval(testds.nclasses(), exp.device, fromfile=metric_location if not replace else None)
    eval_state.miametrics = MIAEval(
        target_model=eval_state.evalmodule, 
        members_loader=DataLoader(trainds.sample_like(testds), exp.hyperparameters.batch_size, True, num_workers=exp.nworkers, persistent_workers=True), 
        nonmembers_loader=DataLoader(testds, exp.hyperparameters.batch_size, True, num_workers=exp.nworkers, persistent_workers=True), 
        device=exp.device, fileloc=metric_location if not replace else None
    )
    eval_state.device = exp.device
    return eval_state

def prepare_hpo(exp: OriginInfo, dr: str, sn: str, study_name: str, storage_loc: str, seed: int) -> optuna.Study:
    already_exists = False
    if os.path.exists(storage_loc): already_exists = True
    study = optuna.create_study(study_name=study_name, storage=f"sqlite:///{storage_loc}", load_if_exists=True)
    if already_exists: return hpo_analysis(study)
    study.optimize(lambda trial: objective(HPOState(), trial), n_trials=exp.hpo.ntrials)
    return study

# core functions #
@no_grad()
def validate(state: TrainState, epoch: int) -> bool:
    state.trainmodule.eval()
    validloss = 0
    for i,(x,y) in enumerate(tqdm(state.validloader, desc=f"Valid {epoch+1}/{state.nepochs}")):
        x, y = x.to(state.device), y.to(state.device)
        yp = state.trainmodule(x)
        loss = state.criterion(yp, y)
        validloss+=loss.item()
    validloss /= len(state.validloader)
    return state.monitor.new_metric(validloss, silent=False), validloss

def train(state: TrainState, logger: DynamicLogger) -> TorchModule: 
    criterion = CrossEntropyLoss()
    state.trainmodule.to(state.device)
    for epoch in range(state.nepochs):
        # Train Step
        state.trainmodule.train()
        trainloss = 0
        for i,(x,y) in enumerate(tqdm(state.trainloader, desc=f"Train {epoch+1}/{state.nepochs}")):
            state.optimizer.zero_grad()
            x, y = x.to(state.device), y.to(state.device)
            yp = state.trainmodule(x)
            loss: Tensor = criterion(yp, y)
            loss.backward()
            state.optimizer.step()
            trainloss += loss.item()
        trainloss /= len(state.trainloader)
        
        # Validation Step
        stop_condition, validloss = validate(state, epoch)        
        if logger is not None: logger.new_metrics(["Origins_Training"], [{"train_loss": trainloss, "validation_loss": validloss}], log_step=True)
        if stop_condition: break
    
    state.trainmodule.load_state_dict(deepcopy(state.monitor.best_state))
    return state.trainmodule.to("cpu")

@no_grad()
def evaluate(state: EvalState, logger: DynamicLogger) -> EvaluationBase:
    if not state.metrics.loaded:
        state.evalmodule.to(state.device).eval()
        for i,(x,y) in enumerate(tqdm(state.testloader, desc="Test")):
            x, y = x.to(state.device), y.to(state.device)
            yp = state.evalmodule(x)
            state.metrics(yp, y)
    
    state.evalmodule.to("cpu")
    logmetrics = {k: v[0] for k,v in state.metrics.todict().items()} | {k: v[0] for k,v in state.miametrics.todict(nfolds=10).items()}
    if logger is not None: logger.new_metrics(["Origins_Evaluations"], [logmetrics])
    print(logmetrics)
    return [state.metrics, state.miametrics]

def objective(state: HPOState, trial: optuna.Trial): pass
def hpo_analysis(study: optuna.Study)->optuna.Study: return study