import os, sys, optuna
from dataclasses import dataclass
from torch.nn import Module as TorchModule
from torchvision.transforms import Compose, ToTensor, Resize
from torch import optim, no_grad, Tensor, load as torch_load, nonzero as torch_nonzero, cat as torch_cat, rand_like as torch_randlike, \
    randint_like as torch_randintlike, tensor as torch_tensor, float as torch_float
from torch.utils.data import DataLoader
from tqdm import tqdm
from copy import deepcopy
from numpy import ndarray

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")))
from src.utils.ConfigTypes import ClassicInfo
from src.utils.addons import DynamicLogger, EarlyStopping
from src.utils.Interfaces import EvaluationBase
from src.models import ClassifierModel, HyCUSBase
from src.metrics import UnlearnEval, MIAEval, HyCUSEval
from src.datasets.loaders import load_icus_dataset, load_classification_fsets, load_semantics
from src.datasets.wrappers import ICUSDataset
from src.implementations.unlearn.finetune import evaluate as common_unlearn_eval, prepare_evaluate as common_unlearn_prepeval

@dataclass
class TrainState():
    trainmodule: HyCUSBase = None
    originmodule: ClassifierModel = None
    trainloader: DataLoader = None
    validloader: DataLoader = None
    optimizer: optim.Optimizer = None
    monitor: EarlyStopping = None
    evalmetrics: UnlearnEval = None
    forgetset: list[int] = None
    retainset: list[int] = None
    logevery: int = 0
    nepochs: int = 0
    device: str = "cpu"

@dataclass
class EvalState():
    evalmodule: ClassifierModel = None
    golden: ClassifierModel = None
    testloader: DataLoader = None
    umetrics: UnlearnEval = None
    miametrics: MIAEval = None
    hycusmetrics: HyCUSEval = None
    device: str = "cpu"

@dataclass
class HPOState(): pass

# preparation functions #
def prepare_train(exp: ClassicInfo, dr: str, sn: str) -> TrainState:
    # state preparation
    trainstate: TrainState = TrainState()
    splits_loc = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "image_splits", sn)
    forget_loc = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "class_splits", f"{exp.method.forget}.txt")
    unseen_loc = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "class_splits", f"{exp.method.origin}.txt")
    classes_loc = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "classes.csv")
    origin_loc = os.path.join("out", "checkpoints", sn, "origin", exp.method.origin, f"{exp.classifier.name}_{exp.dataset.name}.pt")
    semants_loc = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "embeddings", f"{exp.method.embeddings}.npy")


    # dataset and dataloader preparation
    transform = Compose([Resize((exp.classifier.input_size, exp.classifier.input_size)), ToTensor()])
    _, _, fvalidds = load_classification_fsets(splits_loc, unseen_loc, forget_loc, transform)
    trainstate.originmodule = ClassifierModel(exp.classifier, fvalidds.nclasses(), False)
    trainstate.originmodule.load_state_dict(torch_load(origin_loc))
    trainds: ICUSDataset = load_icus_dataset(forget_loc, unseen_loc, classes_loc, semants_loc, trainstate.originmodule)

    # state composition
    trainstate.trainmodule = HyCUSBase(trainds.weights_dimension, trainds.semants_dimension, 512, exp.method.hyperparameters.align_coeff)
    train_bsize = exp.method.hyperparameters.batch_size if not exp.method.strict else 1
    trainstate.trainloader = DataLoader(trainds, train_bsize, True, num_workers=exp.nworkers, persistent_workers=True)
    trainstate.validloader = DataLoader(fvalidds.on_full(), exp.method.hyperparameters.batch_size, False, num_workers=exp.nworkers, persistent_workers=True)
    trainstate.optimizer = getattr(optim, exp.method.hyperparameters.optimizer.name)(trainstate.trainmodule.parameters(), **exp.method.hyperparameters.optimizer.params)
    trainstate.evalmetrics = UnlearnEval(trainds.nclasses, exp.device, fvalidds.translate_labels(fvalidds.forget))
    trainstate.forgetset = trainstate.evalmetrics.forget # in domain [0, nseen)
    trainstate.retainset = trainstate.evalmetrics.retain # in domain [0, nseen)
    trainstate.monitor = EarlyStopping(trainstate.trainmodule, exp.method.hyperparameters.patience)
    trainstate.nepochs = exp.method.hyperparameters.max_epochs
    trainstate.device = exp.device
    trainstate.logevery = exp.logevery
    return trainstate

def prepare_evaluate(exp: ClassicInfo, in_module: TorchModule, dr: str, sn: str, replace: bool) -> EvalState:
    tempstate = TrainState()
    originloc = os.path.join("out", "checkpoints", sn, "origin", exp.method.origin, f"{exp.classifier.name}_{exp.dataset.name}.pt")
    semants_loc = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "embeddings", f"{exp.method.embeddings}.npy")
    unseen_loc = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "class_splits", f"{exp.method.origin}.txt")
    classes_loc = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "classes.csv")
    semantics: Tensor = torch_tensor(load_semantics(semants_loc, classes_loc, unseen_loc), dtype=torch_float)
    tempstate.trainmodule = in_module
    gsm: Tensor = tempstate.trainmodule.make_semants(semantics, device=exp.device)
    tempstate.originmodule = ClassifierModel(exp.classifier, exp.dataset.ntotal_class-exp.dataset.nunseen_test, False)
    tempstate.originmodule.load_state_dict(torch_load(originloc))
    owd: Tensor = tempstate.originmodule.eval().get_classifier_weights()
    ows: Tensor = tempstate.originmodule.eval().get_backbone_weights(["layer4.1.bn2"])
    tempstate.device = exp.device
    unlearnmodel, gws, gwd = unlearn(tempstate, noeval=True)
    evalstate: EvalState = common_unlearn_prepeval(exp, unlearnmodel, dr, sn, replace)
    golden_wd: Tensor = evalstate.golden.get_classifier_weights()
    golden_ws: Tensor = evalstate.golden.get_backbone_weights(["layer4.1.bn2"])
    evalstate.hycusmetrics = HyCUSEval(gws, gwd, ows, owd, golden_ws, golden_wd, gsm, semantics, evalstate.umetrics.forget, device=exp.device)
    return evalstate


def prepare_hpo(exp: ClassicInfo, dr: str, sn: str, study_name: str, storage_loc: str, seed: int) -> optuna.Study:
    already_exists = False
    if os.path.exists(storage_loc): already_exists = True 
    study = optuna.create_study(study_name=f"{study_name}", storage=f"sqlite:///{storage_loc}", load_if_exists=True)
    if already_exists: return hpo_analysis(study) 
    study.optimize(lambda trial: objective(HPOState(), trial), n_trials=exp.method.hpo.ntrials)
    study.set_user_attr("study_seed", seed)
    return study

# core functions #
@no_grad()
def unlearn(state: TrainState, noeval: bool = False) -> ClassifierModel:
    state.trainmodule.eval()
    unlearnmodule = deepcopy(state.originmodule).to("cpu").eval() # deepcopy also copies device location
    wd = unlearnmodule.get_classifier_weights()
    ws = unlearnmodule.get_backbone_weights(keep_list=["layer4.1.bn2"]).unsqueeze(0).repeat(wd.shape[0], 1)
    shared_bias = ws.shape[1] // 2

    # make_weights will compute on state.device and return back on cpu when its done
    gws, gwd = state.trainmodule.make_weights(torch_cat((ws, wd), dim=1), ws.shape[1], aggregation='none', device=state.device)
    unlearnmodule.model.fc.weight[:, :] = gwd[:, :-1]; unlearnmodule.model.fc.bias[:] = gwd[:, -1]
    unlearnmodule.model.layer4[1].bn2.weight[:] = gws[:, :shared_bias].mean(dim=0)
    unlearnmodule.model.layer4[1].bn2.bias[:] = gws[:, shared_bias:].mean(dim=0)
    
    if noeval: return unlearnmodule.to("cpu"), gws.to("cpu"), gwd.to("cpu")

    state.evalmetrics.zero()
    unlearnmodule = unlearnmodule.to(state.device).eval()
    for x,y in tqdm(state.validloader, desc=f"Evaluating Injected Module"):
        x,y = x.to(state.device), y.to(state.device)
        yp = unlearnmodule(x)
        state.evalmetrics(yp, y)
    print({k: f"{v[0]*100:.2f}" if v[1] else f"{v[0]:.4f}" for k,v in state.evalmetrics.todict(train=True).items()})
    return unlearnmodule.to("cpu")

def train(state: TrainState, logger: DynamicLogger, desc: str = "Train") -> TorchModule:
    retainset_tensor = torch_tensor(state.retainset, device=state.device, requires_grad=False)
    state.trainmodule.to(state.device)

    for epoch in range(state.nepochs):
        state.trainmodule.train()
        epochloss = 0
        for isf, a, ws, wd, y in tqdm(state.trainloader, desc=f"HyCUS Training Epoch {epoch+1}"):
            state.optimizer.zero_grad()
            isf, a, ws, wd, y = isf.to(state.device), a.to(state.device), ws.to(state.device), wd.to(state.device), y.to(state.device)
            forget_indexes = torch_nonzero(isf).squeeze(1)
            random_relabel = torch_randintlike(forget_indexes, 0, len(state.retainset), requires_grad=False)
            tgtws = deepcopy(ws); tgtwd = deepcopy(wd); tgtsm = deepcopy(a)
            tgtwd[forget_indexes, :] = state.originmodule.get_classifier_weights()[retainset_tensor[random_relabel].to("cpu"), :].to(state.device)
            tgtws[forget_indexes, :] = torch_randlike(tgtws[forget_indexes, :])
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
        logmetrics = {"Classic/Loss": epochloss, "Classic/Epoch": epoch}
        if (epoch+1) % state.logevery == 0 or stop_condition: unlearn(state); logmetrics = logmetrics | {f"Classic/{k}": v[0] for k,v in state.evalmetrics.todict(train=True).items()}
        logger.new_metrics(["HyCUS_Variants_Train"], [logmetrics], log_step=True)
        if stop_condition: break
    
    state.trainmodule.load_state_dict(deepcopy(state.monitor.best_state))
    return state.trainmodule.to("cpu")

@no_grad()
def evaluate(state: EvalState, logger: DynamicLogger, desc: str = "Evaluate") -> EvaluationBase:
    state.umetrics, state.miametrics = common_unlearn_eval(state, None)
    if logger is not None: 
        resultdict = state.umetrics.todict() | state.miametrics.todict() | state.hycusmetrics.todict()
        logger.new_metrics(["HyCUS_Variants_Eval"], [{f"Classic/{k}": v[0] for k,v in resultdict.items()}])
        print("Metrics:\n", {k: f"{v[0]*100:.2f}%" if v[1] else f"{v[0]:.4f}" for k,v in resultdict.items() if not isinstance(v[0], ndarray)})
    return [state.umetrics, state.miametrics, state.hycusmetrics]

def objective(state: HPOState, trial: optuna.Trial): pass
def hpo_analysis(study: optuna.Study) -> optuna.Study: return study
