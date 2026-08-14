import os, hydra

from torch import save as torch_save, load as torch_load
from torch.nn import Module
from torch.random import manual_seed as torch_seed
from numpy.random import seed as npyseed
from importlib import import_module
from logging import info

from src.utils.ConfigTypes import HydraConf
from src.utils.Interfaces import EvaluationBase
from src.utils.addons import DynamicLogger

def set_and_log_seed(seed, dsname) -> int:
    if seed == -1:
        random_data = os.urandom(4)
        seed = int.from_bytes(random_data, byteorder="big")
    torch_seed(seed)
    npyseed(seed)
    info(f"${dsname}> seed: {seed}")
    return seed

def dispatch(cfg: HydraConf, seed: int):
    allowed_tasks = ["train", "test", "hpo", "evaluate", "hfdownload"]
    assert cfg.task in allowed_tasks, f"task must be one of [train, test, hpo, evaluate, hfdownload]"
    module = f"experiments.{cfg.experiment.name}"
    module = import_module(module)
    
    group_name = f"{cfg.experiment.method.name}_{cfg.experiment.method.origin}" if "method" in cfg.experiment else cfg.experiment.unseen
    run = [cfg.img_split, cfg.experiment.name, group_name]
    branch = f"{cfg.experiment.classifier.name}_{cfg.experiment.dataset.name}"
    if cfg.experiment.name == "unlearn": branch = f"{cfg.experiment.method.forget}_{branch}"
    if cfg.experiment.name == "zslearn": branch = f"{cfg.experiment.method.embeddings}_{branch}"
    if cfg.experiment.name == "hycus": branch = f"{cfg.experiment.method.forget}_{cfg.experiment.method.embeddings}_{branch}"

    if cfg.task == "hpo": return module.hpo(cfg.experiment, cfg.data_root, run, branch, seed) # logs study into .db files during run
    if cfg.task == "test": return module.test(cfg.experiment, cfg.data_root, run, branch)
    if cfg.task == "hfdownload": 
        module.hfdownload(cfg.experiment, cfg.data_root, run, branch) # downloads weights from huggingface
        torch_module = module.load(cfg.experiment, cfg.data_root, run, branch)
        torch_module.load_state_dict(torch_load(os.path.join("out", "checkpoints", *run, f"{branch}.pt")))
        torch_module.requires_grad_(False)
        metrics = module.evaluate(cfg.experiment, torch_module, cfg.data_root, cfg.img_split, None, replace=True) # creates eval files

    if cfg.task == "train" or cfg.task == "evaluate":
        wandb_group = f"{cfg.experiment.name}_{group_name}"
        if cfg.task == "evaluate": wandb_group = f"{wandb_group}_analysis"
        with DynamicLogger(cfg.experiment.runlog, wandb_group, '_'.join(run), branch) as logger:
            if cfg.task == "train": 
                torch_module: Module = module.train(cfg.experiment, cfg.data_root, cfg.img_split, logger)
                if os.path.exists(os.path.join("out", "checkpoints", *run, f"{branch}.pt")): os.remove(os.path.join("out", "checkpoints", *run, f"{branch}.pt"))
                os.makedirs(os.path.join("out", "checkpoints", *run), 777, exist_ok=True)
                torch_save(torch_module.state_dict(), os.path.join("out", "checkpoints", *run, f"{branch}.pt"))
            if cfg.task == "evaluate":
                torch_module = module.load(cfg.experiment, cfg.data_root, run, branch) 
                torch_module.load_state_dict(torch_load(os.path.join("out", "checkpoints", *run, f"{branch}.pt")))
            torch_module.requires_grad_(False)  
            metrics = module.evaluate(cfg.experiment, torch_module, cfg.data_root, cfg.img_split, logger, replace=cfg.task=="train")
    
    os.makedirs(os.path.join("out", "evaluations", *run), 777, exist_ok=True)
    if isinstance(metrics, list):
        for metric in metrics: metric.save(os.path.join("out", "evaluations", *run, branch))
        return None
    
    return metrics.save(os.path.join("out", "evaluations", *run, branch))
        

@hydra.main(version_base='1.3', config_path='./config', config_name='conf')
def main(cfg: HydraConf):
    argseed = cfg.seed
    if "method" in cfg.experiment: argseed = cfg.experiment.method.seeds[cfg.experiment.dataset.name] if "seeds" in cfg.experiment.method else argseed
    else: argseed = cfg.experiment.seeds[cfg.experiment.dataset.name] if "seeds" in cfg.experiment else argseed
    seed = set_and_log_seed(argseed, cfg.experiment.dataset.name)
    return dispatch(cfg, seed)

if __name__ == '__main__':
    main()