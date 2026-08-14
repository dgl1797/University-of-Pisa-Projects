from dataclasses import dataclass
from typing import Literal, Any

## GENERICS ##

@dataclass
class DatasetInfo():
    name: str
    ntotal_class: int
    nunseen_test: int
    nunseen_vali: int
    nimages: int

@dataclass
class LoggerInfo():
    name: str
    path: str
    entity: str
    project: str

@dataclass
class HPInfo():
    @dataclass
    class __OPTInfo():
        name: str
        params: Any
    max_epochs: int
    patience: int
    batch_size: int
    optimizer: __OPTInfo

@dataclass
class HPOInfo():
    ntrials: int

@dataclass
class MethodInfo():
    name: str
    origin: str
    forget: str
    embeddings: str
    hyperparameters: HPInfo
    hpo: HPOInfo

@dataclass
class ClassifierInfo():
    name: str
    weights: str
    input_size: int

@dataclass
class ExperimentInfo():
    runlog: LoggerInfo
    method: MethodInfo
    classifier: ClassifierInfo
    dataset: DatasetInfo
    name: str
    nworkers: int
    device: str
    hyperparameters: HPInfo
    hpo: HPOInfo

@dataclass
class ZSLMUExperimentInfo():
    runlog: LoggerInfo
    dataset: DatasetInfo
    classifier: ClassifierInfo
    method: MethodInfo
    name: str
    nworkers: int
    device: str
    store_best: bool
    logevery: int

@dataclass
class HydraConf():
    experiment: ExperimentInfo
    seed: int 
    task: Literal["train", "evaluate", "hpo", "test"]
    data_root: str
    img_split: str

## METHODS PECIFIC ##

@dataclass
class OriginInfo(ExperimentInfo):
    unseen: str
@dataclass
class RetrainInfo(ZSLMUExperimentInfo):
    pass

@dataclass
class FinetuneInfo(ZSLMUExperimentInfo):
    pass

@dataclass
class NeggradInfo(ZSLMUExperimentInfo):
    class __MethodInfo(MethodInfo):
        zero_accuracy: bool
    method: __MethodInfo

@dataclass
class NeggradplusInfo(ZSLMUExperimentInfo):
    class __MethodInfo(MethodInfo):
        class __HPInfo(HPInfo):
            ngred: float
            pgred: float
        hyperparameters: __HPInfo
    method: __MethodInfo

@dataclass
class BadtInfo(ZSLMUExperimentInfo):
    class __MethodInfo(MethodInfo):
        class __HPInfo(HPInfo):
            kltemp: float
            retainp: float
            rred: float
            fred: float
        hyperparameters: __HPInfo
    method: __MethodInfo

@dataclass
class ScrubInfo(ZSLMUExperimentInfo):
    class __MethodInfo(MethodInfo):
        class __HPInfo(HPInfo):
            kltemp: float
            klw: float
            cew: float
        hyperparameters: __HPInfo
    method: __MethodInfo

@dataclass
class ConseInfo(ZSLMUExperimentInfo):
    class __MethodInfo(MethodInfo):
        embeddings: str
    method: __MethodInfo

@dataclass
class CostaInfo(ZSLMUExperimentInfo):
    class __MethodInfo(MethodInfo):
        class __HPInfo(HPInfo):
            restore_lr: float
            restore_weight_decay: float
        centroids: str
        hyperparameters: __HPInfo
    method: __MethodInfo

@dataclass
class ClassicInfo(ZSLMUExperimentInfo):
    class __MethodInfo(MethodInfo):
        class __HPInfo(HPInfo):
            align_coeff: float
        hyperparameters: __HPInfo
        strict: bool
    method: __MethodInfo

@dataclass
class NoisingInfo(ZSLMUExperimentInfo):
    class __MethodInfo(MethodInfo):
        class __HPInfo(HPInfo):
            align_coeff: float
        hyperparameters: __HPInfo
        strict: bool
    method: __MethodInfo

@dataclass
class L1SparseInfo(ZSLMUExperimentInfo):
    class __MethodInfo(MethodInfo):
        class __HPInfo(HPInfo):
            alpha: float
            noL1Epochs: int
        hyperparameters: __HPInfo
    method: __MethodInfo