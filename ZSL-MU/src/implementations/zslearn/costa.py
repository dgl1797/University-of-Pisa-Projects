import os, sys, optuna
from dataclasses import dataclass
from torch.nn import Module as TorchModule, Linear
from torch import optim, no_grad, Tensor, load as torch_load
from torch.utils.data import DataLoader
from tqdm import tqdm
from numpy import ndarray, load as npyload
from copy import deepcopy

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")))
from src.implementations.zslearn.conse import evaluate as common_zslearn_eval, prepare_evaluate as common_zslearn_prepeval
from src.utils.ConfigTypes import CostaInfo
from src.utils.addons import DynamicLogger
from src.utils.Interfaces import EvaluationBase
from src.datasets.loaders import load_classification_sets
from src.models import ZSLPredictor, ClassifierModel
from src.metrics import ZSLearnEval

class COSTA(ZSLPredictor):
    def __init__(self, base: ClassifierModel, seen: list[int], unseen: list[int], attributes: ndarray, device: str, gzsl: bool = False):
        super().__init__(base, seen, unseen, attributes, device, gzsl)
    
    def zsl(self):
        predictor = deepcopy(self.base).to(self.device); sattrs = self.seen_attributes.to(self.device); cattrs = self.candidate_attributes.to(self.device)
        coocsms: Tensor = sattrs.T.matmul(cattrs)/sattrs.sum(dim=0).unsqueeze(1)
        known_weights: Tensor = predictor.model.fc.weight.T
        known_biases: Tensor = predictor.model.fc.bias
        unkwn_weights: Tensor = known_weights.matmul(coocsms)
        unkwn_biases: Tensor = known_biases.matmul(coocsms)
        newhead = Linear(predictor.model.fc.in_features, unkwn_weights.shape[1], device=self.device)
        newhead.weight[:, :] = unkwn_weights.T[:, :]; newhead.bias[:] = unkwn_biases[:]
        predictor.model.fc = newhead
        self.predictor = predictor.to("cpu").requires_grad_(False).eval()
        return self
    
    def gzsl(self):
        predictor = deepcopy(self.base).to(self.device); sattrs = self.seen_attributes.to(self.device); cattrs = self.candidate_attributes.to(self.device)
        coocsms: Tensor = sattrs.T.matmul(cattrs)/sattrs.sum(dim=0).unsqueeze(1)
        known_weights: Tensor = predictor.model.fc.weight.T
        known_biases: Tensor = predictor.model.fc.bias
        unkwn_weights: Tensor = known_weights.matmul(coocsms)
        unkwn_biases: Tensor = known_biases.matmul(coocsms)
        newhead = Linear(predictor.model.fc.in_features, known_weights.shape[1] + unkwn_weights.shape[1], device=self.device)
        # seen weights
        newhead.weight[[i-self.correction for i in self.seen_cids_map], :] = known_weights.T[:, :]
        newhead.bias[[i-self.correction for i in self.seen_cids_map]] = known_biases[:]
        # unseen weights
        newhead.weight[[i-self.correction for i in self.unsn_cids_map], :] = unkwn_weights.T[:, :]
        newhead.bias[[i-self.correction for i in self.unsn_cids_map]] = unkwn_biases[:]
        # predictor setup
        predictor.model.fc = newhead
        self.predictor = predictor.to("cpu").requires_grad_(False).eval()
        return self

    @no_grad()
    def predict(self, x: Tensor) -> Tensor:
        x = x.to(self.device); predictor = self.predictor.to(self.device)
        return predictor(x).argmax(dim=1)

@dataclass
class TrainState():
    trainmodule: TorchModule = None

@dataclass
class EvalState():
    evalmodule: COSTA = None
    evalmetrics: ZSLearnEval = None
    zslloader: DataLoader = None
    gzslloader: DataLoader = None
    device: str = "cpu"

@dataclass
class HPOState(): pass

# preparation functions #
def prepare_train(exp: CostaInfo, dr: str, sn: str) -> TrainState:
    split_loc = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "image_splits", sn)
    unseen_fl = os.path.join(dr, f"{exp.dataset.name.upper()}_Data", "class_splits", f"{exp.method.origin}.txt")
    trainds, _, _ = load_classification_sets(split_loc, unseen_fl, None)
    full_attributes = npyload(os.path.join(dr, f"{exp.dataset.name}_Data", "embeddings", f"{exp.method.embeddings}.npy"))
    origin_classifier = ClassifierModel(exp.classifier, trainds.nclasses(), finetune=False)
    originloc = os.path.join("out", "checkpoints", sn, "origin", exp.method.origin, f"{exp.classifier.name}_{exp.dataset.name}.pt")
    origin_classifier.load_state_dict(torch_load(originloc))
    return TrainState(
        trainmodule=COSTA(
            base=origin_classifier,
            seen=trainds.cids_map,
            unseen=trainds.unseen,
            attributes=full_attributes,
            device=exp.device
        )
    )

def prepare_evaluate(exp: CostaInfo, in_module: TorchModule, dr: str, sn: str, replace: bool) -> EvalState:
    return common_zslearn_prepeval(exp, in_module, dr, sn, replace)

def prepare_hpo(exp: CostaInfo, dr: str, sn: str, study_name: str, storage_loc: str, seed: int) -> optuna.Study:
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
    state.evalmetrics = common_zslearn_eval(state, None)
    if logger is not None: logger.new_metrics(["ZSLearn_Baselines"], [{f"COSTA/{k}": v[0] for k,v in state.evalmetrics.todict().items()}])
    return state.evalmetrics

def objective(state: HPOState, trial: optuna.Trial): pass
def hpo_analysis(study: optuna.Study) -> optuna.Study: return study
