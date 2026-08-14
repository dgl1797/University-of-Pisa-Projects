import os, sys, optuna
from dataclasses import dataclass
from torch.nn import Module as TorchModule
from torch.nn.functional import kl_div
from torch import optim, no_grad, load as torch_load, tensor as torch_tensor, where
from torch.utils.data import DataLoader
from tqdm import tqdm
from copy import deepcopy
from torchvision.transforms import Compose, Resize, ToTensor

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")))
from src.utils.ConfigTypes import BadtInfo
from src.utils.addons import DynamicLogger, EarlyStopping
from src.utils.Interfaces import EvaluationBase
from src.datasets.loaders import load_classification_fsets
from src.datasets.wrappers import MULDataset
from src.models import ClassifierModel
from src.metrics import UnlearnEval, MIAEval
from src.implementations.unlearn.finetune import prepare_evaluate as common_unlearn_prepeval, evaluate as common_unlearn_eval,\
    prepare_train as common_unlearn_preptrain, validate as common_unlearn_validate

@dataclass
class TrainState():
    trainmodule: ClassifierModel = None
    goodteacher: ClassifierModel = None
    badteacher: ClassifierModel = None
    trainloader: DataLoader = None
    validloader: DataLoader = None
    optimizer: optim.Optimizer = None
    monitor: EarlyStopping = None
    forgetset: list[int] = None
    evalmetrics: UnlearnEval = None
    nepochs: int = 0
    kltemp: float = 1.0
    rred: float = 1.0
    fred: float = 1.0
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
class HPOState():
    classifier_info: dict = None
    nclasses: int = 0
    goodteacher: TorchModule = None
    badteacher: TorchModule = None
    forgetset: list[int] = None
    device: str = "cpu"
    testloader: DataLoader = None
    hpometrics: UnlearnEval = None
    trainds: MULDataset = None
    nworkers: int = 0

# preparation functions #
def prepare_train(exp: BadtInfo, dr: str, sn: str) -> TrainState:
    trainstate: TrainState = common_unlearn_preptrain(exp, dr, sn)
    trainds: MULDataset = trainstate.trainloader.dataset

    # trainstate composition
    trainstate.goodteacher = ClassifierModel(exp.classifier, trainds.nclasses(), finetune=False)
    trainstate.goodteacher.load_state_dict(deepcopy(trainstate.trainmodule.state_dict()))
    trainstate.badteacher = ClassifierModel(exp.classifier, trainds.nclasses(), finetune=False).randinit()
    trainstate.optimizer = getattr(optim, exp.method.hyperparameters.optimizer.name)(trainstate.trainmodule.parameters(), **exp.method.hyperparameters.optimizer.params)
    trainstate.trainloader = DataLoader((exp.method.hyperparameters.retainp*trainds).on_full(), exp.method.hyperparameters.batch_size, True, num_workers=exp.nworkers, persistent_workers=True)
    trainstate.forgetset = trainstate.evalmetrics.forget
    trainstate.kltemp = float(exp.method.hyperparameters.kltemp)
    trainstate.rred = exp.method.hyperparameters.rred
    trainstate.fred = exp.method.hyperparameters.fred[exp.dataset.name]
    return trainstate

def prepare_evaluate(exp: BadtInfo, in_module: TorchModule, dr: str, sn: str, replace: bool) -> EvalState:
    return common_unlearn_prepeval(exp, in_module, dr, sn, replace)

def prepare_hpo(exp: BadtInfo, dr: str, sn: str, study_name: str, storage_loc: str, seed: int) -> optuna.Study:
    split_loc = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "image_splits", sn)
    unseen_fl = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "class_splits", f"{exp.method.origin}.txt")
    forget_fl = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "class_splits", f"{exp.method.forget}.txt")
    transform = Compose([Resize((exp.classifier.input_size, exp.classifier.input_size)), ToTensor()])
    trainds, testds, _ = load_classification_fsets(split_loc, unseen_fl, forget_fl, transform)
    steacher = ClassifierModel(exp.classifier, trainds.nclasses(), finetune=False)
    obranch = f"{exp.classifier.name}_{exp.dataset.name}.pt"
    steacher.load_state_dict(torch_load(os.path.join("out", "checkpoints", sn, "origin", exp.method.origin, obranch)))
    hpomodel = ClassifierModel(exp.classifier, trainds.nclasses(), finetune=True)
    hpomodel.load_state_dict(deepcopy(steacher.state_dict()))
    already_exists = False
    if os.path.exists(storage_loc): already_exists = True 
    study = optuna.create_study(
        study_name=f"{study_name}", storage=f"sqlite:///{storage_loc}", direction="maximize", load_if_exists=True,
        pruner=optuna.pruners.MedianPruner(), sampler=optuna.samplers.TPESampler()
    )
    if already_exists: return hpo_analysis(study)
    study.optimize(lambda trial: objective(HPOState(
        classifier_info=exp.classifier,
        nclasses=trainds.nclasses(),
        goodteacher=steacher.to(exp.device),
        baadteacher=ClassifierModel(exp.classifier, trainds.nclasses(), finetune=False).randinit().to(exp.device),
        forgetset=trainds.translate_labels(trainds.forget),
        device=exp.device,
        testloader=DataLoader(testds.on_full(), exp.method.hyperparameters.batch_size, False, num_workers=exp.nworkers, persistent_workers=True),
        hpometrics=UnlearnEval(trainds.nclasses(), exp.device, trainds.translate_labels(trainds.forget)),
        trainds=trainds, nworkers=exp.nworkers
    ), trial), n_trials=exp.method.hpo.ntrials)
    study.set_user_attr("study_seed", seed)
    return study

# core functions #
def train(state: TrainState, logger: DynamicLogger, desc: str = "Train") -> TorchModule:
    state.trainmodule.to(state.device); state.goodteacher.to(state.device); state.badteacher.to(state.device)
    for epoch in range(state.nepochs):
        state.trainmodule.train()
        for x,y in tqdm(state.trainloader, desc=f"Unlearn {epoch+1}/{state.nepochs}"):
            state.optimizer.zero_grad()
            x,y = x.to(state.device), y.to(state.device)
            pu = (state.trainmodule(x)/state.kltemp).log_softmax(dim=1)
            with no_grad():
                ps = (state.goodteacher(x)/state.kltemp).softmax(dim=1) # smart prob space
                pd = (state.badteacher(x)/state.kltemp).softmax(dim=1) # dumbt prob space
                mask = torch_tensor([True if lbl in state.forgetset else False for lbl in y], device=state.device, requires_grad=False)
            # kl_div with reduction='none' computes the KL(p,t) t_i * log(t_i) - p_i) for each element, p must be log_softmax
            loss = where(mask, state.fred*kl_div(pu, pd, reduction='none').sum(dim=1), state.rred*kl_div(pu, ps, reduction='none').sum(dim=1)).mean()
            loss.backward()
            state.optimizer.step()

        stop_condition = common_unlearn_validate(state, epoch)
        if logger is not None: logger.new_metrics(["Unlearn_Baselines_Training"], [{f"BadT/{k}": v[0] for k,v in state.evalmetrics.todict(train=True).items()}], log_step=True)
        if stop_condition: break
    
    state.trainmodule.load_state_dict(deepcopy(state.monitor.best_state))
    state.goodteacher.to("cpu"); state.badteacher.to("cpu")
    return state.trainmodule.to("cpu")

@no_grad()
def evaluate(state: EvalState, logger: DynamicLogger) -> EvaluationBase:
    state.umetrics, state.miametrics = common_unlearn_eval(state, None)
    resultdict = state.umetrics.todict() | state.miametrics.todict()
    if logger is not None: logger.new_metrics(["Unlearn_Baselines"], [{f"BadT/{k}": v[0] for k,v in resultdict.items()}])
    print("Metrics:\n", {k: f"{v[0]*100:.2f}%" if v[1] else f"{v[0]:.4f}" for k,v in resultdict.items()})
    return [state.umetrics, state.miametrics]

def objective(state: HPOState, trial: optuna.Trial):
    # trail setup
    nepochs = trial.suggest_int("nepochs", 1, 10)
    retain_percentage = trial.suggest_float("retainp", 0.3, 1.0)
    learning_rate = trial.suggest_float("lr", 1e-8, 1e-3)
    bsize = trial.suggest_int("bsize", 1,32)
    trainloader = DataLoader((retain_percentage*state.trainds).on_full(), bsize, False, num_workers=state.nworkers, persistent_workers=True)
    hpomodule = ClassifierModel(state.classifier_info, state.nclasses, finetune=True).to(state.device)
    hpomodule.load_state_dict(deepcopy(state.goodteacher.state_dict()))
    optimizer = optim.AdamW(hpomodule.parameters(), lr=learning_rate)
    
    # trial execution
    for epoch in tqdm(range(nepochs), desc="Trial"):
        hpomodule.train()
        for x,y in trainloader:
            optimizer.zero_grad()
            x,y = x.to(state.device), y.to(state.device)
            yu = hpomodule(x)
            pu = yu.log_softmax(dim=1)
            with no_grad():
                ps = (state.goodteacher(x)).softmax(dim=1) # smart prob space
                pd = (state.baadteacher(x)).softmax(dim=1) # dumbt prob space
                mask = torch_tensor([True if lbl in state.forgetset else False for lbl in y], device=state.device, requires_grad=False)
            # kl_div with reduction='none' computes the KL(p,t) t_i * log(t_i) - p_i) for each element, p must be log_softmax
            loss = where(mask, kl_div(pu, pd, reduction='none').sum(dim=1), kl_div(pu, ps, reduction='none').sum(dim=1)).mean()
            loss.backward()
            optimizer.step()
            state.hpometrics(yu, y)
        
        intermediate_value = state.hpometrics.accuracy(retain=True)-state.hpometrics.accuracy(retain=False)
        trial.report(intermediate_value, step=epoch)
        if trial.should_prune(): raise optuna.TrialPruned()
    
    hpomodule.eval()
    with no_grad():
        for x,y in state.testloader:
            x,y = x.to(state.device), y.to(state.device)
            yu = hpomodule(x)
            state.hpometrics(yu,y)
    
    return state.hpometrics.accuracy(retain=True) - state.hpometrics.accuracy(retain=False)

def hpo_analysis(study: optuna.Study) -> optuna.Study: return study