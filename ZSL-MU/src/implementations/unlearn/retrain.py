import os, sys, optuna
from dataclasses import dataclass
from torchvision.transforms import Compose, Resize, ToTensor
from torch.nn import Module as TorchModule, CrossEntropyLoss
from torch import optim, no_grad, Tensor
from torch.utils.data import DataLoader
from tqdm import tqdm
from numpy import ndarray

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")))
from src.utils.ConfigTypes import RetrainInfo
from src.utils.addons import DynamicLogger, EarlyStopping
from src.utils.Interfaces import EvaluationBase
from src.models import ClassifierModel
from src.datasets.loaders import load_classification_fsets
from src.metrics import ClassificationEval, UnlearnEval, MIAEval
from src.implementations.origin import train as origin_train

@dataclass
class TrainState():
    trainmodule: ClassifierModel = None
    trainloader: DataLoader = None
    validloader: DataLoader = None
    optimizer: optim.Optimizer = None
    monitor: EarlyStopping = None
    criterion: TorchModule = None
    nepochs: int = 0
    device: str = "cpu"

@dataclass
class EvalState():
    evalmodule: ClassifierModel = None
    testloader: DataLoader = None
    umetrics: UnlearnEval = None
    miametrics: MIAEval = None
    device: str = "cpu"

@dataclass
class HPOState(): pass

# preparation functions #
def prepare_train(exp: RetrainInfo, dr: str, sn: str) -> TrainState:
    trainstate = TrainState(); dsdir = f"{exp.dataset.name.upper()}_Data"
    split_loc = os.path.join(dr, dsdir, "image_splits", sn)
    unseen_file = os.path.join(dr, dsdir, "class_splits", f"{exp.method.origin}.txt")
    forget_file = os.path.join(dr, dsdir, "class_splits", f"{exp.method.forget}.txt")
    transform = Compose([Resize((exp.classifier.input_size, exp.classifier.input_size)), ToTensor()])
    trainds, _, validds = load_classification_fsets(split_loc, unseen_file, forget_file, transform)
    assert trainds.nclasses() == exp.dataset.ntotal_class - exp.dataset.nunseen_test, f"bad split: train dataset is missing some seen classes"
    assert validds.nclasses() == exp.dataset.ntotal_class - exp.dataset.nunseen_test, f"bad split: valid dataset is missing some seen classes"

    # trainstate composition
    trainstate.trainmodule = ClassifierModel(exp.classifier, trainds.nclasses(), True)
    trainstate.optimizer = getattr(optim, exp.method.hyperparameters.optimizer.name)(
        trainstate.trainmodule.parameters(), **exp.method.hyperparameters.optimizer.params
    )
    # passing entire trainstate.trainmodule reference to monitor so that trainmodule can move its parameters freerly to cuda and cpu
    trainstate.monitor = EarlyStopping(trainstate.trainmodule, exp.method.hyperparameters.patience)
    trainstate.trainloader = DataLoader(trainds, exp.method.hyperparameters.batch_size, True, num_workers=exp.nworkers, persistent_workers=True)
    trainstate.validloader = DataLoader(validds, exp.method.hyperparameters.batch_size, False, num_workers=exp.nworkers, persistent_workers=True)
    trainstate.nepochs = exp.method.hyperparameters.max_epochs
    trainstate.device = exp.device
    trainstate.criterion = CrossEntropyLoss()
    return trainstate

def prepare_evaluate(exp: RetrainInfo, in_module: TorchModule, dr: str, sn: str, replace: bool) -> EvalState:
    dsdir = f"{exp.dataset.name.upper()}_Data"
    split_loc = os.path.join(dr, dsdir, "image_splits", sn)
    unseen_file = os.path.join(dr, dsdir, "class_splits", f"{exp.method.origin}.txt")
    forget_file = os.path.join(dr, dsdir, "class_splits", f"{exp.method.forget}.txt")
    transform = Compose([Resize((exp.classifier.input_size, exp.classifier.input_size)), ToTensor()])
    trainds, testds, _ = load_classification_fsets(split_loc, unseen_file, forget_file, transform)
    forgetbsize = int(round(exp.method.hyperparameters.batch_size * (len(trainds.forget) / len(trainds.cids_map))))
    assert testds.nclasses() == exp.dataset.ntotal_class - exp.dataset.nunseen_test, f"bad split: test dataset is missing some seen classes"
    
    # origin retain accuracy
    originloc = os.path.join("out", "evaluations", sn, "origin", exp.method.origin, f"{exp.classifier.name}_{exp.dataset.name}")
    origin_cm: ndarray = ClassificationEval(testds.nclasses(), exp.device, fromfile=originloc).get_state()
    retain_indexes = [i for i in range(testds.nclasses()) if i not in testds.translate_labels(testds.forget)]
    rtps: ndarray = origin_cm.diagonal()[retain_indexes].sum()
    rall: ndarray = origin_cm.sum(axis=1)[retain_indexes].sum()
    origin_racc = (rtps/rall).item() if rall != 0.0 else 1.0

    # unlearn metrics
    metricloc = os.path.join("out", "evaluations", sn, "unlearn", f"{exp.method.name}_{exp.method.origin}", f"{exp.method.forget}_{exp.classifier.name}_{exp.dataset.name}")
    unlearn_metrics = UnlearnEval(
        testds.nclasses(), exp.device, testds.translate_labels(testds.forget), origin_racc, fromfile=metricloc if not replace else None
    )
    
    # mia attack preparation
    mialoc = os.path.join("out", "evaluations", sn, "unlearn", f"{exp.method.name}_{exp.method.origin}", f"{exp.method.forget}_{exp.classifier.name}_{exp.dataset.name}")
    mia_eval = MIAEval(
        target_model = in_module, 
        members_loader = DataLoader(trainds.sample_like(testds).on_forget(), forgetbsize, True, num_workers=exp.nworkers, persistent_workers=True),
        nonmembers_loader = DataLoader(testds.on_forget(), forgetbsize, True, num_workers=exp.nworkers, persistent_workers=True),
        fileloc = mialoc if not replace else None,
        device = exp.device
    )

    # state composition
    return EvalState(
        evalmodule=in_module,
        testloader=DataLoader(testds.on_full(), exp.method.hyperparameters.batch_size, False, num_workers=exp.nworkers, persistent_workers=True),
        umetrics=unlearn_metrics,
        miametrics=mia_eval,
        device=exp.device
    )

def prepare_hpo(exp: RetrainInfo, dr: str, sn: str, study_name: str, storage_loc: str, seed: int) -> optuna.Study:
    already_exists = False
    if os.path.exists(storage_loc): already_exists = True
    study = optuna.create_study(study_name=study_name, storage=f"sqlite:///{storage_loc}", load_if_exists=True)
    if already_exists: return hpo_analysis(study)
    study.optimize(lambda trial: objective(HPOState(), trial), n_trials=exp.hpo.ntrials)
    return study

# core functions #
def train(state: TrainState, logger: DynamicLogger) -> TorchModule:
    # origin_train will move the model stored as reference in state to cuda
    return origin_train(state, None).to("cpu")

@no_grad()
def evaluate(state: EvalState, logger: DynamicLogger) -> EvaluationBase:
    if not state.umetrics.loaded:
        state.evalmodule.to(state.device).eval()
        for i,(x,y) in enumerate(tqdm(state.testloader, desc="Evaluation")):
            x: Tensor = x.to(state.device); y: Tensor = y.to(state.device)
            yp = state.evalmodule(x)
            state.umetrics(yp, y)

    state.evalmodule.to("cpu")
    resultdict = state.umetrics.todict() | state.miametrics.todict(nfolds=10)
    if logger is not None: logger.new_metrics(["Unlearn_Baselines"], [{f"Retrain/{k}": v[0] for k,v in resultdict.items()}])
    print("Metrics:\n", {k: f"{v[0]*100:.2f}%" if v[1] else f"{v[0]:.4f}" for k,v in resultdict.items()})
    
    return [state.umetrics, state.miametrics]

def objective(state: HPOState, trial: optuna.Trial): pass
def hpo_analysis(study: optuna.Study)->optuna.Study: return study