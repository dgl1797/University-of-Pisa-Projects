import os, sys, optuna
from dataclasses import dataclass
from torch.nn import Module as TorchModule, CrossEntropyLoss
from torchvision.transforms import Compose, Resize, ToTensor
from torch import optim, no_grad, load as torch_load
from torch.utils.data import DataLoader
from tqdm import tqdm
from typing import Any
from copy import deepcopy

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")))
from src.utils.ConfigTypes import NeggradInfo
from src.utils.addons import DynamicLogger, EarlyStopping
from src.utils.Interfaces import EvaluationBase
from src.datasets.loaders import load_classification_fsets
from src.models import ClassifierModel
from src.datasets.wrappers import MULDataset
from src.metrics import UnlearnEval, MIAEval
from src.implementations.unlearn.finetune import prepare_evaluate as common_unlearn_prepeval, evaluate as common_unlearn_eval,\
    validate as common_unlearn_validate, prepare_train as common_unlearn_preptrain

@dataclass
class TrainState():
    trainmodule: ClassifierModel = None
    trainloader: DataLoader = None
    validloader: DataLoader = None
    fvalidloader: DataLoader = None
    optimizer: optim.Optimizer = None
    evalmetrics: UnlearnEval = None
    monitor: EarlyStopping = None
    nepochs: int = 0
    device: str = "cpu"
    criterion: TorchModule = None

@dataclass
class EvalState():
    evalmodule: ClassifierModel = None
    golden: ClassifierModel = None
    testloader: DataLoader = None
    umetrics: UnlearnEval = None
    miametrics: MIAEval = None
    device: str = "cpu"

@dataclass
class HPOState():
    classifier_info: Any = None
    nclasses: int = 0
    classifier_loc: str = None
    forgetset: list[int] = None
    hpotforget: MULDataset = None
    hpovonfull: DataLoader = None
    hpotonfull: DataLoader = None
    metrics: UnlearnEval = None
    nepochs: int = 0
    patience: int = 0
    nworkers: int = 4
    device: str = "cpu"

# preparation functions #
def prepare_train(exp: NeggradInfo, dr: str, sn: str) -> TrainState:
    trainstate: TrainState = common_unlearn_preptrain(exp, dr, sn)
    trainds: MULDataset = trainstate.trainloader.dataset
    validds: MULDataset = trainstate.validloader.dataset

    for i in range(len(trainstate.optimizer.param_groups)): 
        trainstate.optimizer.param_groups[i]['lr'] = exp.method.hyperparameters[exp.dataset.name].lr
    trainstate.trainloader = DataLoader(trainds.on_forget(), exp.method.hyperparameters[exp.dataset.name].batch_size, True, num_workers=exp.nworkers, persistent_workers=True)
    trainstate.fvalidloader = DataLoader(validds.on_forget(), exp.method.hyperparameters.batch_size, True, num_workers=exp.nworkers, persistent_workers=True)
    trainstate.monitor = EarlyStopping(trainstate.trainmodule, exp.method.hyperparameters.patience, minimize=exp.method.zero_accuracy)
    trainstate.criterion = CrossEntropyLoss()
    return trainstate

def prepare_evaluate(exp: NeggradInfo, in_module: TorchModule, dr: str, sn: str, replace: bool) -> EvalState:
    return common_unlearn_prepeval(exp, in_module, dr, sn, replace)

def prepare_hpo(exp: NeggradInfo, dr: str, sn: str, study_name: str, storage_loc: str, seed: int) -> optuna.Study:
    split_loc = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "image_splits", sn)
    unseen_fl = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "class_splits", f"{exp.method.origin}.txt")
    forget_fl = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "class_splits", f"{exp.method.forget}.txt")
    transform = Compose([Resize((exp.classifier.input_size, exp.classifier.input_size)), ToTensor()])
    trainds, testds, validds = load_classification_fsets(split_loc, unseen_fl, forget_fl, transform)
    hpostate = HPOState(
        classifier_info=exp.classifier,
        nclasses=trainds.nclasses(),
        forgetset=trainds.translate_labels(trainds.forget),
        classifier_loc=os.path.join("out", "checkpoints", sn, "origin", exp.method.origin, f"{exp.classifier.name}_{exp.dataset.name}.pt"),
        hpotforget=trainds.on_forget(),
        hpotonfull=DataLoader(testds.on_full(), exp.method.hyperparameters.batch_size, False, num_workers=exp.nworkers, persistent_workers=True),
        hpovonfull=DataLoader(validds.on_full(), exp.method.hyperparameters.batch_size, False, num_workers=exp.nworkers, persistent_workers=True),
        device=exp.device,
        patience=exp.method.hyperparameters.patience,
        nepochs=exp.method.hyperparameters.max_epochs
    )
    already_exists = False
    if os.path.exists(storage_loc): already_exists = True
    study = optuna.create_study( # change back to f"{study_name}" for unrevised version
        study_name=f"{study_name}_revised", storage=f"sqlite:///{storage_loc}", direction="maximize", load_if_exists=True,
        sampler=optuna.samplers.TPESampler()
    )
    if already_exists: return hpo_analysis(study)
    study.optimize(lambda trial: objective(hpostate, trial), n_trials=exp.method.hpo.ntrials)
    study.set_user_attr("study_seed", seed)
    return study

# core functions #
@no_grad()
def validate(state: TrainState, epoch: int) -> bool: # facc only
    state.trainmodule.eval()
    state.evalmetrics.zero()
    for x,y in tqdm(state.fvalidloader, desc=f"Validating {epoch+1}/{state.nepochs}"):
        x,y = x.to(state.device), y.to(state.device)
        yp = state.trainmodule(x)
        state.evalmetrics(yp, y)
    facc = state.evalmetrics.accuracy(retain=False)
    return state.monitor.new_metric(facc, silent=False, strict=True) or facc == 0.0

def train(state: TrainState, logger: DynamicLogger) -> TorchModule:
    criterion = CrossEntropyLoss(); state.trainmodule.to(state.device)
    for epoch in range(state.nepochs):
        state.trainmodule.train()
        for x,y in tqdm(state.trainloader, desc=f"Unlearn {epoch+1}/{state.nepochs}"):
            state.optimizer.zero_grad()
            x,y = x.to(state.device), y.to(state.device)
            yp = state.trainmodule(x)
            loss = -criterion(yp, y)
            loss.backward()
            state.optimizer.step()
        
        stop_condition = validate(state, epoch) if state.monitor.minimize else common_unlearn_validate(state, epoch)
        if logger is not None: logger.new_metrics(["Unlearn_Baselines_Training"], [{f"NegGrad/{k}": v[0] for k,v in state.evalmetrics.todict(train=True).items()}], log_step=True)
        if stop_condition: break
    
    state.trainmodule.load_state_dict(deepcopy(state.monitor.best_state))
    return state.trainmodule.to("cpu")

@no_grad()
def evaluate(state: EvalState, logger: DynamicLogger) -> EvaluationBase:
    state.umetrics, state.miametrics = common_unlearn_eval(state, None)
    resultdict = state.umetrics.todict() | state.miametrics.todict()
    if logger is not None: logger.new_metrics(["Unlearn_Baselines"], [{f"NegGrad/{k}": v[0] for k,v in resultdict.items()}])
    print("Metrics:\n", {k: f"{v[0]*100:.2f}%" if v[1] else f"{v[0]:.4f}" for k,v in resultdict.items()})
    return [state.umetrics, state.miametrics]

def objective(state: HPOState, trial: optuna.Trial):
    hpomodel = ClassifierModel(state.classifier_info, state.nclasses, finetune=True).to(state.device)
    hpomodel.load_state_dict(torch_load(state.classifier_loc))
    monitor = EarlyStopping(hpomodel, state.patience, minimize=True)
    metrics = UnlearnEval(state.nclasses, state.device, state.forgetset)
    criterion = CrossEntropyLoss()
    
    nepochs = state.nepochs
    learnrt = trial.suggest_float("learning_rate", 1e-8, 1e-5)
    optimizer: optim.Optimizer = getattr(optim, "Adam")(hpomodel.parameters(), lr=learnrt)
    bchsize = trial.suggest_int("bsize", 1,32)
    forget_trainldr = DataLoader(state.hpotforget, bchsize, False, num_workers=state.nworkers, persistent_workers=True)
    
    for _ in tqdm(range(nepochs), desc="Trial Epochs"):
        hpomodel.train()
        for x,y in forget_trainldr:
            optimizer.zero_grad()
            x,y = x.to(state.device), y.to(state.device)
            yp = hpomodel(x)
            loss = -criterion(yp, y)
            loss.backward()
            optimizer.step()
        
        with no_grad():
            metrics.zero()
            hpomodel.eval()
            for x,y in state.hpovonfull:
                x,y = x.to(state.device), y.to(state.device)
                yp = hpomodel(x)
                metrics(yp, y)
        if monitor.new_metric(metrics.accuracy(retain=True) - metrics.accuracy(retain=False), strict=True): break
    
    hpomodel.load_state_dict(deepcopy(monitor.best_state))
    with no_grad():
        metrics.zero()
        hpomodel.eval()
        for x,y in state.hpotonfull:
            x,y = x.to(state.device), y.to(state.device)
            yp = hpomodel(x)
            metrics(yp, y)
        delta_accuracies = metrics.accuracy(retain=True) - metrics.accuracy(retain=False)
    return delta_accuracies
    

def hpo_analysis(study: optuna.Study)->optuna.Study:
    from plotly.io import show as plotly_show
    fig = optuna.visualization.plot_pareto_front(
        study, targets=lambda t: (t.values[0], t.values[1]),
        target_names=["Forget Accuracy", "Cross Entropy Loss"]
    )
    plotly_show(fig)
    seed = study.user_attrs["study_seed"]
    print(seed)
    return study