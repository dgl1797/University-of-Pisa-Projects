from dataclasses import dataclass
from omegaconf import MISSING
from pathlib import Path

@dataclass
class SANE_TRAINING():
    batchsize: int = MISSING
    reduction: str = MISSING
    nepochs: int = MISSING
    earlystop: str = MISSING
    patience: int = MISSING
    windowsize: int = MISSING
    gamma: float = MISSING
    temperature: float = MISSING
    stride: int = MISSING
    nrandom: int = MISSING

@dataclass
class SANE_AE_CONF():
    transformer_type: str = MISSING
    input_dim: int = MISSING
    latent_dim: int = MISSING
    max_positions: list[int] = MISSING
    embedding_dim: int = MISSING
    nhead: int = MISSING
    nblocks: int = MISSING

@dataclass
class SANE_OPTIM_CONF():
    lr: float = MISSING
    wd: float = MISSING
    scheduler: str = MISSING

@dataclass
class PATHS_CONF():
    trainckpt_loc: Path = MISSING
    testchkpt_loc: Path = MISSING
    testimges_loc: Path = MISSING
    testimgfilter: Path = MISSING

@dataclass
class SANE_CONF():
    paths: PATHS_CONF
    device: str = "cpu"
    nworkers: int = MISSING
    training: SANE_TRAINING = MISSING
    ae: SANE_AE_CONF = MISSING
    optimizer: SANE_OPTIM_CONF = MISSING