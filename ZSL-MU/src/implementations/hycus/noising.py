import os, sys, optuna
from dataclasses import dataclass
from torch.nn import Module as TorchModule
from torch import optim, no_grad, Tensor, nonzero as torch_nonzero, cat as torch_cat
from torch.utils.data import DataLoader
from tqdm import tqdm
from copy import deepcopy

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")))
from src.utils.ConfigTypes import NoisingInfo
from src.utils.addons import DynamicLogger, EarlyStopping, gaussian_noise
from src.utils.Interfaces import EvaluationBase
from src.models import ClassifierModel, HyCUSBase
from src.metrics import UnlearnEval, MIAEval, HyCUSEval
from src.implementations.hycus.classic import prepare_train as common_hycus_preptrain, unlearn as common_hycus_unlearn, \
    prepare_evaluate as common_hycus_prepeval, evaluate as common_hycus_eval

@dataclass
class TrainState():
    trainmodule: HyCUSBase = None
    originmodule: ClassifierModel = None
    trainloader: DataLoader = None
    validloader: DataLoader = None
    optimizer: optim.Optimizer = None
    monitor: EarlyStopping = None
    evalmetrics: UnlearnEval = None
    nepochs: int = 0
    logevery: int = 0
    device: str = "cpu"
    oldorigws: Tensor = None
    oldorigwd: Tensor = None

@dataclass
class EvalState():
    evalmodule: ClassifierModel = None
    expert: ClassifierModel = None
    testloader: DataLoader = None
    umetrics: UnlearnEval = None
    miametrics: MIAEval = None
    hycusmetrics: HyCUSEval = None
    device: str = "cpu"

@dataclass
class HPOState(): pass

# preparation functions #
def prepare_train(exp: NoisingInfo, dr: str, sn: str) -> TrainState:
    trainstate: TrainState = common_hycus_preptrain(exp, dr, sn)
    return trainstate

def prepare_evaluate(exp: NoisingInfo, in_module: TorchModule, dr: str, sn: str, replace: bool) -> EvalState:
    return common_hycus_prepeval(exp, in_module, dr, sn, replace)

def prepare_hpo(exp: NoisingInfo, dr: str, sn: str, study_name: str, storage_loc: str, seed: int) -> optuna.Study:
    already_exists = False
    if os.path.exists(storage_loc): already_exists = True 
    study = optuna.create_study(study_name=f"{study_name}", storage=f"sqlite:///{storage_loc}", load_if_exists=True)
    if already_exists: return hpo_analysis(study) 
    study.optimize(lambda trial: objective(HPOState(), trial), n_trials=exp.method.hpo.ntrials)
    study.set_user_attr("study_seed", seed)
    return study

def train(state: TrainState, logger: DynamicLogger, desc: str = "Train") -> TorchModule:

    for epoch in range(state.nepochs):
        state.trainmodule.train()
        epochloss = 0
        for isf, a, ws, wd, y in tqdm(state.trainloader, desc=f"HyCUS Training Epoch {epoch+1}"):
            state.optimizer.zero_grad()
            isf, a, ws, wd, y = isf.to(state.device), a.to(state.device), ws.to(state.device), wd.to(state.device), y.to(state.device)
            retain_indexes = torch_nonzero(~isf).squeeze(1)
            tgtws = deepcopy(ws); tgtwd = deepcopy(wd); tgtsm = deepcopy(a)
            tgtwd[retain_indexes, :] = gaussian_noise(tgtwd[retain_indexes, :], mean=0.0, std=1e-1)
            tgtws[retain_indexes, :] = gaussian_noise(tgtws[retain_indexes, :], mean=0.0, std=1e-6)
            a.requires_grad_(True)
            weights = torch_cat((ws, wd), dim=1).requires_grad_(True)
            tgtweights = torch_cat((tgtws, tgtwd), dim=1)
            wtow, stos, wtos, stow, z_weight, z_semant = state.trainmodule((weights, a))
            loss = state.trainmodule.loss(wtow, stos, wtos, stow, z_weight, z_semant, tgtweights, tgtsm)
            loss.backward()
            state.optimizer.step()
            epochloss += loss.item()
        epochloss /= len(state.trainloader)
        
        stop_condition = state.monitor.new_metric(epochloss, silent=False, strict=True)
        logmetrics = {"Noising/Loss": epochloss, "Noising/Epoch": epoch}
        if (epoch+1) % state.logevery == 0 or stop_condition: common_hycus_unlearn(state); logmetrics = logmetrics | {f"Noising/{k}": v[0] for k,v in state.evalmetrics.todict(train=True).items()}
        logger.new_metrics(["HyCUS_Variants_Train"], [logmetrics], log_step=True)
        if stop_condition: break
    
    state.trainmodule.load_state_dict(deepcopy(state.monitor.best_state))
    return state.trainmodule.to("cpu")

@no_grad()
def evaluate(state: EvalState, logger: DynamicLogger, desc: str = "Evaluate") -> EvaluationBase:
    state.umetrics, state.miametrics, state.hycusmetrics = common_hycus_eval(state, None)
    if logger is not None: 
        resultdict = state.umetrics.todict() | state.miametrics.todict() | state.hycusmetrics.todict()
        logger.new_metrics(["HyCUS_Variants"], [{f"Noising/{k}": v[0] for k,v in resultdict.items()}])
        print("Metrics:\n", {k: f"{v[0]*100:.2f}%" if v[1] else f"{v[0]:.4f}" for k,v in resultdict.items()})
    return [state.umetrics, state.miametrics, state.hycusmetrics]

def objective(state: HPOState, trial: optuna.Trial): pass
def hpo_analysis(study: optuna.Study) -> optuna.Study: return study
