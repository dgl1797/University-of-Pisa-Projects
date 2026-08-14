from wandb import init as wandb_init
from .config_manager import use_config
from .schemas.loggers.wandb import WANDB_LOG

@use_config("loggers/wandb", WANDB_LOG)
def get_wandb(conf: WANDB_LOG, name: str, group: str): return wandb_init(**conf, name=name, group=group)