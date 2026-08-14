import os,sys
from torch.nn import Module
from math import inf
from copy import deepcopy
from torch import rand_like as torch_randlike, Tensor
from wandb.plot import histogram as wandb_histogram
from wandb import Table as WBTable
from numpy import ndarray

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from utils.ConfigTypes import LoggerInfo

class EarlyStopping():
    def __init__(self, model: Module, patience: int, store_best: bool = True, minimize: bool = True, threshold: float = 0):
        self.store_best = store_best
        self.best_metric = inf if minimize else -inf
        self.minimize = minimize
        self.threshold = threshold
        self.model = model
        self.best_state = deepcopy(model.state_dict())
        self.patience = patience
        self.patience_counter = 0

    def __store_new_state(self, metric_value, silent: bool):
        if not silent: print(f"New best metric: {metric_value:e}")
        self.patience_counter = 0
        self.best_metric = metric_value
        if self.store_best: self.best_state = deepcopy(self.model.state_dict())
        return False # break the loop = False

    def new_metric(self, metric_value: float, silent: bool = True, strict: bool = False) -> bool:
        if strict: return self.__new_metric_strict(metric_value, silent)
        if self.minimize and metric_value + self.threshold <= self.best_metric: # further training at parity of metric
            return self.__store_new_state(metric_value, silent)
        if not self.minimize and metric_value - self.threshold >= self.best_metric: # further training at parity of metric
            return self.__store_new_state(metric_value, silent)
        self.patience_counter+=1
        if not self.store_best: self.best_state = deepcopy(self.model.state_dict())
        if self.patience_counter == self.patience: return True # break the loop = True
        return False
    def __new_metric_strict(self, metric_value: float, silent: bool = True) -> bool:
        if self.minimize and metric_value + self.threshold < self.best_metric:
            return self.__store_new_state(metric_value, silent)
        if not self.minimize and metric_value - self.threshold > self.best_metric:
            return self.__store_new_state(metric_value, silent)
        self.patience_counter+=1
        if not self.store_best: self.best_state = deepcopy(self.model.state_dict())
        if self.patience_counter == self.patience: return True
        return False
    def reduce_counter(self)->int:
        if self.patience_counter > 0: self.patience_counter -= 1
        return self.patience_counter
    def reset_counter(self)->None:
        self.patience_counter = 0
        return None
    def reset(self):
        self.patience_counter = 0
        self.best_state = deepcopy(self.model.state_dict())
        self.best_metric = inf if self.minimize else -inf

class DynamicLogger():
    def __init__(self, logger_info: LoggerInfo, wbgrp: str, run: str, branch: str):
        self.logger_name = logger_info.name
        self.path = os.path.join(*logger_info.path.split("/"))
        self.step: int = 0
        if logger_info.name == "wandb":
            import wandb
            self.logger = wandb.init(
                entity=logger_info.entity, project=logger_info.project, 
                dir=logger_info.path, name=f"{run}_{branch}",
                group=wbgrp
            )
        if logger_info.name == "tensorboard":
            from torch.utils.tensorboard.writer import SummaryWriter
            self.logger = SummaryWriter(os.path.join(logger_info.path, run, branch))

    def __enter__(self):
        return self
    def __exit__(self, exc_type, exc_value, traceback):
        if exc_type: print(exc_value); print(traceback)
        self.stop()
        return False
    def __del__(self):
        self.stop()
        return None
    
    def new_metrics(self, groups: list[str], metrics: list[dict[str, float]], log_step: bool = False):
        if self.logger_name == "tensorboard":
            for i in range(len(groups)): 
                group_scalars = {mk: mv for mk,mv in metrics[i].items() if not isinstance(mv, ndarray)}
                group_histogs = {mk: mv for mk,mv in metrics[i].items() if isinstance(mv, ndarray)}
                self.logger.add_scalars(groups[i], group_scalars, self.step if log_step else 0)
                [self.logger.add_histogram(f"{groups[i]}/{ghk}", ghv, self.step if log_step else 0) for ghk,ghv in group_histogs.items()]

        if self.logger_name == "wandb":
            loggroup = {f"{g}/{mk}": wandb_histogram(WBTable(data=[[x] for x in mv], columns=["values"]), value="values", title=mk) if isinstance(mv, ndarray) else mv for g,m in zip(groups, metrics) for mk,mv in m.items()}
            self.logger.log(loggroup, step=self.step if log_step else None)
        
        if log_step: self.step += 1
        return None

    def reset_step(self, new_step: int = None)->int:
        last_step = self.step
        self.step = 0 if new_step is None else new_step
        return last_step

    def stop(self):
        if self.logger_name == "wandb": self.logger.finish()
        if self.logger_name == "tensorboard": self.logger.close()
        return None

def gaussian_noise(original: Tensor, mean: float = 0.0, std: float = 1e-1):
    return original + (torch_randlike(original) * std + mean)