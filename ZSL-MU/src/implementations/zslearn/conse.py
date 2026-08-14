import os, sys, optuna
from dataclasses import dataclass
from torch.nn import Module as TorchModule
from torch import no_grad, load as torch_load, Tensor, zeros as torch_zeros, float as torch_float
from numpy import ndarray
from torch.utils.data import DataLoader
from tqdm import tqdm
from numpy import load as npyload
from torchvision.transforms import Compose, Resize, ToTensor
from typing import Any

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")))
from src.utils.ConfigTypes import ConseInfo
from src.utils.addons import DynamicLogger
from src.utils.Interfaces import EvaluationBase
from src.metrics import ZSLearnEval
from src.models import ClassifierModel, ZSLPredictor
from src.datasets.loaders import load_classification_sets

class ConSE(ZSLPredictor):
    def __init__(self, base: ClassifierModel, seen: list[int], unseen: list[int], attributes: ndarray, device: str, gzsl: bool = False):
        super().__init__(base, seen, unseen, attributes, device, gzsl)

    @no_grad()
    def predict(self, x: Tensor) -> Tensor:
        x = x.to(self.device); sattrs = self.seen_attributes.to(self.device); cattrs = self.candidate_attributes.to(self.device)
        probs: Tensor = self.base(x).softmax(dim=1) # shape [nb, nsc]

        #sattrs [na, nsc] => sattrs.T [nsc, na] resulting in sum(p[i,j]*semantic[:,j]) foreach batch, Z = 1 as it sums all probabilities
        conses: Tensor = probs.matmul(sattrs.T)        
        conses_normalized = conses / conses.norm(dim=1, keepdim=True) # normalizing each row vector by its norm
        cattrs_normalized = cattrs / cattrs.norm(dim=0, keepdim=True) # normalizing each column vector by its norm
        cossims: Tensor = conses_normalized.matmul(cattrs_normalized) # shape [nb, nuc]
        return cossims.argmax(dim=1).to("cpu")

@dataclass
class TrainState():
    trainmodule: ConSE = None

@dataclass
class EvalState():
    evalmodule: ConSE = None
    evalmetrics: ZSLearnEval = None
    zslloader: DataLoader = None
    gzslloader: DataLoader = None
    device: str = "cpu"

@dataclass
class HPOState(): pass


# preparation functions #
def prepare_train(exp: ConseInfo, dr: str, sn: str) -> TrainState:
    split_loc = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "image_splits", sn)
    unseen_fl = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "class_splits", f"{exp.method.origin}.txt")
    trainds, _, _ = load_classification_sets(split_loc, unseen_fl, None)
    full_attributes = npyload(os.path.join(dr, f"{exp.dataset.name}_Data", "embeddings", f"{exp.method.embeddings}.npy"))
    origin_classifier = ClassifierModel(exp.classifier, trainds.nclasses(), finetune=False)
    originloc = os.path.join("out", "checkpoints", sn, "origin", exp.method.origin, f"{exp.classifier.name}_{exp.dataset.name}.pt")
    origin_classifier.load_state_dict(torch_load(originloc))
    return TrainState(
        trainmodule=ConSE(
            base=origin_classifier,
            seen=trainds.cids_map,
            unseen=trainds.unseen,
            attributes=full_attributes,
            device=exp.device
        )
    )

def prepare_evaluate(exp: ConseInfo, in_module: ConSE, dr: str, sn: str, replace: bool) -> EvalState:
    split_loc = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "image_splits", sn)
    unseen_fl = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "class_splits", f"{exp.method.origin}.txt")
    transform = Compose([Resize((exp.classifier.input_size, exp.classifier.input_size)), ToTensor()])
    _, _, testds = load_classification_sets(split_loc, unseen_fl, transform)
    unseen_bsize = int(round(exp.method.hyperparameters.batch_size * (testds.on_unseen().nclasses()/testds.nclasses())))
    return EvalState(
        evalmodule=in_module,
        evalmetrics = ZSLearnEval(exp.dataset.ntotal_class, in_module.seen_cids_map, in_module.unsn_cids_map, exp.device),
        zslloader=DataLoader(testds.on_unseen(), unseen_bsize, False, num_workers=exp.nworkers, persistent_workers=True),
        gzslloader=DataLoader(testds.on_full(), exp.method.hyperparameters.batch_size, False, num_workers=exp.nworkers, persistent_workers=True),
        device=exp.device
    )

def prepare_hpo(exp: ConseInfo, dr: str, sn: str, study_name: str, storage_loc: str, seed: int) -> optuna.Study:
    already_exists = False
    if os.path.exists(storage_loc): already_exists = True 
    study = optuna.create_study(study_name=f"{study_name}", storage=f"sqlite:///{storage_loc}", load_if_exists=True)
    if already_exists: return hpo_analysis(study) 
    study.optimize(lambda trial: objective(HPOState(), trial), n_trials=exp.method.hpo.ntrials)
    study.set_user_attr("study_seed", seed)
    return study

# core functions #
def train(state: TrainState, logger: DynamicLogger, desc: str = "Train") -> TorchModule:
    print("this method doesn't require any training, returning the model as it is")
    return state.trainmodule.to("cpu")

@no_grad()
def evaluate(state: EvalState, logger: DynamicLogger) -> EvaluationBase:
    # ZSL
    state.evalmodule.to(state.device).zsl()
    for x,y in tqdm(state.zslloader, desc="ZSL Evaluation"):
        x,y = x.to(state.device), y.to(state.device)
        yp = state.evalmodule.predict(x).to("cpu").apply_(lambda lbl: state.evalmetrics.unseen[lbl]).to(state.device) 
        # both yp and y tensors in unseen space, while evalmetrics on full space
        state.evalmetrics(yp, y.to("cpu").apply_(lambda lbl: state.evalmetrics.unseen[lbl]).to(state.device))
    state.evalmetrics.store_accuracy()

    # GZSL
    state.evalmodule.gzsl()
    state.evalmetrics.zero()
    for x,y in tqdm(state.gzslloader, desc="GZSL Evaluation"):
        x,y = x.to(state.device), y.to(state.device)
        yp = state.evalmodule.predict(x)
        # both yp and y in full space as evalmetrics
        state.evalmetrics(yp, y)
    print(state.evalmetrics)

    if logger is not None: logger.new_metrics(["ZSLearn_Baselines"], [{f"ConSE/{k}": v[0] for k,v in state.evalmetrics.todict().items()}])
    return state.evalmetrics

def objective(state: HPOState, trial: optuna.Trial): pass
def hpo_analysis(study: optuna.Study) -> optuna.Study: return study
