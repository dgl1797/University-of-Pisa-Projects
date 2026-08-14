import os, sys
from importlib import import_module
from torch import load as torch_load
from huggingface_hub import hf_hub_download

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from src.utils.ConfigTypes import OriginInfo

def train(exp: OriginInfo, data_root: str, img_split: str, logger):
    implementation = import_module(f"src.implementations.{exp.name}")
    state = implementation.prepare_train(exp, data_root, img_split)
    origin_module = implementation.train(state, logger)
    return origin_module

def hfdownload(exp: OriginInfo, data_root: str, run: list[str], branch: str):
    model_loc = os.path.join("out", "checkpoints", *run, f"{branch}.pt")
    if os.path.exists(model_loc): os.remove(model_loc)
    os.makedirs(os.path.dirname(model_loc), 777, exist_ok=True)
    hf_hub_download(
        repo_id=f"MarcoParola/mu_{exp.classifier.name}_origin_{exp.dataset.name}",
        filename=f"{branch}.pt",
        local_dir=os.path.dirname(model_loc)
    )

    return model_loc

def load(exp: OriginInfo, data_root: str, run: list[str], branch: str):
    implementation = import_module(f"src.implementations.{exp.name}")
    state = implementation.prepare_train(exp, data_root, run[0])
    state.trainmodule.load_state_dict(torch_load(os.path.join("out", "checkpoints", *run, f"{branch}.pt")))
    return state.trainmodule.to("cpu")

def evaluate(exp: OriginInfo, origin_module, data_root: str, img_split: str, logger, replace: bool = True):
    implementation = import_module(f"src.implementations.{exp.name}")
    state = implementation.prepare_evaluate(exp, origin_module, data_root, img_split, replace)
    origin_metrics = implementation.evaluate(state, logger)
    return origin_metrics

def hpo(exp: OriginInfo, data_root: str, run: list[str], branch: str, seed: int):
    implementation = import_module(f"src.implementations.unlearn.{exp.method.name}")
    storage_loc = f"out/hpos/{'/'.join(run)}/study.db"
    os.makedirs(os.path.dirname(storage_loc), 777, exist_ok=True)
    study_name = branch
    return implementation.prepare_hpo(exp, data_root, run[0], study_name, storage_loc, seed)

def test(exp: OriginInfo, data_root: str, run: list[str], branch: str): pass
