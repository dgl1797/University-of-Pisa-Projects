import os, sys, optuna
from dataclasses import dataclass
from torch.nn import Module as TorchModule
from torch import optim, no_grad, Tensor, tensor as torch_tensor
from torch.nn.functional import kl_div, cross_entropy
from torch.utils.data import DataLoader
from tqdm import tqdm
from copy import deepcopy
from numpy import ndarray

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")))
from src.utils.ConfigTypes import ScrubInfo
from src.utils.addons import DynamicLogger, EarlyStopping
from src.utils.Interfaces import EvaluationBase
from src.datasets.wrappers import MULDataset
from src.models import ClassifierModel
from src.metrics import UnlearnEval, ClassificationEval, MIAEval
from src.implementations.unlearn.finetune import evaluate as common_unlearn_eval, prepare_evaluate as common_unlearn_prepeval,\
    prepare_train as common_unlearn_preptrain, validate as common_unlearn_validate

@dataclass
class TrainState():
    trainmodule: ClassifierModel = None
    teachermodl: ClassifierModel = None
    retainloader: DataLoader = None
    forgetloader: DataLoader = None
    validloader: DataLoader = None
    optimizer: optim.Optimizer = None
    monitor: EarlyStopping = None
    forgetset: list[int] = None
    evalmetrics: UnlearnEval = None
    kltemp: float = 1.0
    klw: float = 1.0
    cew: float = 1.0
    nepochs: int = 0
    prevfacc: float = 0.0
    minforget_epochs = 8
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
def prepare_train(exp: ScrubInfo, dr: str, sn: str) -> TrainState:
    trainstate: TrainState = common_unlearn_preptrain(exp, dr, sn)
    trainds: MULDataset = trainstate.trainloader.dataset
    retain_batchsize = int(round(exp.method.hyperparameters.batch_size * ((trainds.nclasses() - len(trainds.forget))/(trainds.nclasses()))))
    forget_batchsize = int(round(exp.method.hyperparameters.batch_size * (len(trainds.forget) / trainds.nclasses())))
    
    # origin forget accuracy
    origin_evalname = f"{exp.classifier.name}_{exp.dataset.name}"
    originloc = os.path.join("out", "evaluations", sn, "origin", exp.method.origin, origin_evalname)
    origin_cm: ndarray = ClassificationEval(trainds.nclasses(), exp.device, fromfile=originloc).get_state()
    forget_indexes = trainds.translate_labels(trainds.forget)
    rtps: ndarray = origin_cm.diagonal()[forget_indexes].sum()
    rall: ndarray = origin_cm.sum(axis=1)[forget_indexes].sum()
    origin_facc = (rtps/rall).item() if rall != 0.0 else 1.0
    
    # state composition
    trainstate.teachermodl = ClassifierModel(exp.classifier, trainds.nclasses(), finetune=False)
    trainstate.teachermodl.load_state_dict(deepcopy(trainstate.trainmodule.state_dict()))
    trainstate.retainloader=DataLoader(trainds.on_retain(), retain_batchsize, True, num_workers=exp.nworkers, persistent_workers=True)
    trainstate.forgetloader=DataLoader(trainds.on_forget(), forget_batchsize, True, num_workers=exp.nworkers, persistent_workers=True)
    trainstate.forgetset = trainds.translate_labels(trainds.forget)
    trainstate.kltemp = exp.method.hyperparameters.kltemp
    trainstate.klw = exp.method.hyperparameters.klw
    trainstate.cew = exp.method.hyperparameters.cew
    trainstate.prevfacc = origin_facc
    trainstate.minforget_epochs = 15 if exp.dataset.name == "cub" else 0
    return trainstate

def prepare_evaluate(exp: ScrubInfo, in_module: TorchModule, dr: str, sn: str, replace: bool) -> EvalState:
    return common_unlearn_prepeval(exp, in_module, dr, sn, replace)

def prepare_hpo(exp: ScrubInfo, dr: str, sn: str, study_name: str, storage_loc: str, seed: int) -> optuna.Study:
    already_exists = False
    if os.path.exists(storage_loc): already_exists = True 
    study = optuna.create_study(study_name=f"{study_name}", storage=f"sqlite:///{storage_loc}", load_if_exists=True)
    if already_exists: return hpo_analysis(study) 
    study.optimize(lambda trial: objective(HPOState(), trial), n_trials=exp.method.hpo.ntrials)
    study.set_user_attr("study_seed", seed)
    return study

# core functions #
def train(state: TrainState, logger: DynamicLogger, desc: str = "Train") -> TorchModule:
    state.trainmodule.to(state.device); state.teachermodl.to(state.device)
    for epoch in range(state.nepochs):
        state.trainmodule.train()
        if state.prevfacc > 0.0 or epoch < state.minforget_epochs:
            state.monitor.reset_counter()
            for x,y in tqdm(state.forgetloader, desc=f"Forget Epoch {epoch+1}/{state.nepochs}"):
                state.optimizer.zero_grad()
                x,y = x.to(state.device), y.to(state.device)
                pu = (state.trainmodule(x)/state.kltemp).log_softmax(dim=1)
                with no_grad(): pt = (state.teachermodl(x)/state.kltemp).softmax(dim=1)
                loss = - state.klw * kl_div(pu, pt, reduction='none').sum(dim=1).mean()
                loss.backward()
                state.optimizer.step()
        elif epoch == state.minforget_epochs: state.monitor.reset()
        for x,y in tqdm(state.retainloader, desc=f"Retain Epoch {epoch+1}/{state.nepochs}"):
            state.optimizer.zero_grad()
            x,y = x.to(state.device), y.to(state.device)
            yu = state.trainmodule(x)
            pu = (yu/state.kltemp).log_softmax(dim=1)
            with no_grad(): pt = (state.teachermodl(x)/state.kltemp).softmax(dim=1)
            loss = state.klw * kl_div(pu, pt, reduction='none').sum(dim=1).mean() + state.cew * cross_entropy(yu, y) 
            loss.backward()
            state.optimizer.step()
        
        stop_condition = common_unlearn_validate(state, epoch)
        if logger is not None: logger.new_metrics(["Unlearn_Baselines_Training"], [{f"SCRUB/{k}": v[0] for k,v in state.evalmetrics.todict(train=True).items()}], log_step=True)
        if stop_condition: break
        state.prevfacc = state.evalmetrics.accuracy(retain=False)
    
    state.trainmodule.load_state_dict(deepcopy(state.monitor.best_state)); state.teachermodl.to("cpu")
    return state.trainmodule.to("cpu")

@no_grad()
def evaluate(state: EvalState, logger: DynamicLogger) -> EvaluationBase:
    state.umetrics, state.miametrics = common_unlearn_eval(state, None)
    resultdict = state.umetrics.todict() | state.miametrics.todict()
    if logger is not None: logger.new_metrics(["Unlearn_Baselines"], [{f"SCRUB/{k}": v[0] for k,v in resultdict.items()}])
    print("Metrics:\n", {k: f"{v[0]*100:.2f}%" if v[1] else f"{v[0]:.4f}" for k,v in resultdict.items()})
    return [state.umetrics, state.miametrics]

def objective(state: HPOState, trial: optuna.Trial): pass
def hpo_analysis(study: optuna.Study) -> optuna.Study: return study
