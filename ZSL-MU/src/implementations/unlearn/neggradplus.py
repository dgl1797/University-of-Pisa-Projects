import os, sys, optuna
from dataclasses import dataclass
from torch.nn import Module as TorchModule, CrossEntropyLoss
from torch import optim, no_grad, Tensor, tensor as torch_tensor
from torch.utils.data import DataLoader
from tqdm import tqdm
from copy import deepcopy

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")))
from src.utils.ConfigTypes import NeggradplusInfo
from src.utils.addons import DynamicLogger, EarlyStopping
from src.utils.Interfaces import EvaluationBase
from src.datasets.wrappers import MULDataset
from src.metrics import UnlearnEval, MIAEval
from src.models import ClassifierModel
from src.implementations.unlearn.finetune import evaluate as common_unlearn_eval, prepare_evaluate as common_unlearn_prepeval,\
    prepare_train as common_unlearn_preptrain, validate as common_unlearn_validate


@dataclass
class TrainState():
    trainmodule: TorchModule = None
    trainloader: DataLoader = None
    validloader: DataLoader = None
    optimizer: optim.Optimizer = None
    evalmetrics: UnlearnEval = None
    monitor: EarlyStopping = None
    forgetset: list[int] = None
    ngred: float = -1.0
    pgred: float = 1.0
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
def prepare_train(exp: NeggradplusInfo, dr: str, sn: str) -> TrainState:
    trainstate: TrainState = common_unlearn_preptrain(exp, dr, sn)
    trainds: MULDataset = trainstate.trainloader.dataset
    trainstate.trainloader = DataLoader(trainds.on_full(), exp.method.hyperparameters.batch_size, True, num_workers=exp.nworkers, persistent_workers=True)
    trainstate.forgetset = trainstate.evalmetrics.forget
    trainstate.ngred = exp.method.hyperparameters.ngred
    trainstate.pgred = exp.method.hyperparameters.pgred
    return trainstate

def prepare_evaluate(exp: NeggradplusInfo, in_module: TorchModule, dr: str, sn: str, replace: bool) -> EvalState:
    return common_unlearn_prepeval(exp, in_module, dr, sn, replace)

def prepare_hpo(exp: NeggradplusInfo, dr: str, sn: str, study_name: str, storage_loc: str, seed: int) -> optuna.Study:
    already_exists = False
    if os.path.exists(storage_loc): True 
    study = optuna.create_study(study_name=f"{study_name}", storage=f"sqlite:///{storage_loc}", load_if_exists=True)
    if already_exists: return hpo_analysis(study) 
    study.optimize(lambda trial: objective(HPOState(), trial), ntrials=exp.method.hpo.ntrials)
    study.set_user_attr("study_seed", seed)
    return study

# core functions #
def train(state: TrainState, logger: DynamicLogger, desc: str = "Train") -> TorchModule:
    criterion = CrossEntropyLoss(reduction='none'); state.trainmodule.to(state.device)
    for epoch in range(state.nepochs):
        state.trainmodule.train()
        for x,y in tqdm(state.trainloader, desc=f"Unlearn {epoch+1}/{state.nepochs}"):
            state.optimizer.zero_grad()
            x,y = x.to(state.device), y.to(state.device)
            yp = state.trainmodule(x)
            mask = torch_tensor([-state.ngred if lbl in state.forgetset else state.pgred for lbl in y], device=state.device)
            loss: Tensor = (mask * criterion(yp, y)).mean()
            loss.backward()
            state.optimizer.step()
        
        stop_condition = common_unlearn_validate(state, epoch)
        if logger is not None: logger.new_metrics(["Unlearn_Baselines_Training"], [{f"NegGradPlus/{k}": v[0] for k,v in state.evalmetrics.todict(train=True).items()}], log_step=True)
        if stop_condition: break
    
    state.trainmodule.load_state_dict(deepcopy(state.monitor.best_state))
    return state.trainmodule.to("cpu")

@no_grad()
def evaluate(state: EvalState, logger: DynamicLogger) -> EvaluationBase:
    state.umetrics, state.miametrics = common_unlearn_eval(state, None)
    resultdict = state.umetrics.todict() | state.miametrics.todict()
    if logger is not None: logger.new_metrics(["Unlearn_Baselines"], [{f"NegGradPlus/{k}": v[0] for k,v in resultdict.items()}])
    print("Metrics:\n", {k: f"{v[0]*100:.2f}%" if v[1] else f"{v[0]:.4f}" for k,v in resultdict.items()})
    return [state.umetrics, state.miametrics]

def objective(state: HPOState, trial: optuna.Trial): pass
def hpo_analysis(study: optuna.Study) -> optuna.Study: return study
