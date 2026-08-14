import os, sys
from importlib import import_module
from torch import load as torch_load
from huggingface_hub import hf_hub_download

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from src.utils.ConfigTypes import ExperimentInfo

def train(exp: ExperimentInfo, data_root: str, img_split: str, logger):
    implementation = import_module(f"src.implementations.hycus.{exp.method.name}")
    state = implementation.prepare_train(exp, data_root, img_split)
    hycus_module = implementation.train(state, logger)
    return hycus_module

def hfdownload(exp: ExperimentInfo, data_root: str, run: list[str], branch: str):
    model_loc = os.path.join("out", "checkpoints", *run, f"{branch}.pt")
    if os.path.exists(model_loc): os.remove(model_loc)
    os.makedirs(os.path.dirname(model_loc), 777, exist_ok=True)
    hf_hub_download(
        repo_id=f"MarcoParola/mu_{exp.classifier.name}_origin_{exp.dataset.name}",
        filename=f"{branch}.pt",
        local_dir=os.path.dirname(model_loc)
    )

    return model_loc

def load(exp: ExperimentInfo, data_root: str, run: list[str], branch: str):
    implementation = import_module(f"src.implementations.hycus.{exp.method.name}")
    state = implementation.prepare_train(exp, data_root, run[0])
    state.trainmodule.load_state_dict(torch_load(os.path.join("out", "checkpoints", *run, f"{branch}.pt")))
    return state.trainmodule.to("cpu")

def evaluate(exp: ExperimentInfo, hycus_module, data_root: str, img_split: str, logger, replace: bool = True):
    implementation = import_module(f"src.implementations.hycus.{exp.method.name}")
    state = implementation.prepare_evaluate(exp, hycus_module, data_root, img_split, replace)
    hycus_metrics = implementation.evaluate(state, logger)
    return hycus_metrics

def hpo(exp: ExperimentInfo, data_root: str, run: list[str], branch: str, seed: int):
    implementation = import_module(f"src.implementations.hycus.{exp.method.name}")
    storage_loc = f"out/hpos/{'/'.join(run)}/study.db"
    os.makedirs(os.path.dirname(storage_loc), 777, exist_ok=True)
    study_name = f'{branch}'
    return implementation.prepare_hpo(exp, data_root, run[0], study_name, storage_loc, seed)

def test(exp: ExperimentInfo, data_root: str, run: list[str], branch: str):
    import wandb, numpy as np

    data1 = np.random.randn(768)
    data2 = np.random.random(768)

    table_1 = wandb.Table(data=[[i, x] for i,x in enumerate(data1)], columns=["index", "value"])
    table_2 = wandb.Table(data=[[i, x] for i,x in enumerate(data2)], columns=["index", "value"])

    histogram_1 = wandb.plot.histogram(table_1, value="value", title="normal distribution")
    histogram_2 = wandb.plot.histogram(table_2, value="value", title="gaussian distribution")

    with wandb.init(exp.runlog.entity, exp.runlog.project, exp.runlog.path, name=f"{'_'.join(run)}", group="HyCUS_Test") as logger:
        logger.log({"histogram_1": histogram_1, "histogram_2": histogram_2})
    