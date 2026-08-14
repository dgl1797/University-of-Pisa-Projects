import argparse, os

## CREATION TASKS ##

def write_experiment(name: str, group: str):
    import_name = ''.join([c.upper() if i==0 else c for i,c in enumerate(name)]) if group is None else "Experiment"
    
    storage_string = """f\"out/hpos/{'/'.join(run)}/study.db\""""
    module_string = """f\"src.implementations.{exp.name}\"""" if group is None else f"f\"src.implementations.{group}."+"{exp.method.name}\""
    load_modstring = """os.path.join(\"out\", \"checkpoints\", *run, f"{branch}.pt")"""

    file_string = f"""import os, sys
from importlib import import_module
from torch import load as torch_load
from huggingface_hub import hf_hub_download

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from src.utils.ConfigTypes import {import_name}Info

def train(exp: {import_name}Info, data_root: str, img_split: str, logger):
    implementation = import_module({module_string})
    state = implementation.prepare_train(exp, data_root, img_split)
    {name if group is None else group}_module = implementation.train(state, logger)
    return {name if group is None else group}_module

def hfdownload(exp: {import_name}Info, data_root: str, run: list[str], branch: str):
    model_loc = os.path.join("out", "checkpoints", *run, {"f'{branch}.pt'"})
    
    if os.path.exists(model_loc): os.remove(model_loc)
    os.makedirs(os.path.dirname(model_loc), 777, exist_ok=True)
    
    hf_hub_download(
        repo_id={"f'MarcoParola/mu_{exp.classifier.name}_origin_{exp.dataset.name}'"},
        filename={"f'{branch}.pt'"},
        local_dir=os.path.dirname(model_loc)
    )

    return model_loc

def load(exp: {import_name}Info, data_root: str, run: list[str], branch: str):
    implementation = import_module({module_string})
    state = implementation.prepare_train(exp, data_root, run[0])
    state.trainmodule.load_state_dict(torch_load({load_modstring}))
    return state.trainmodule.to("cpu")

def evaluate(exp: {import_name}Info, {name if group is None else group}_module, data_root: str, img_split: str, logger, replace: bool = True):
    implementation = import_module({module_string})
    state = implementation.prepare_evaluate(exp, {name if group is None else group}_module, data_root, img_split, replace)
    {name if group is None else group}_metrics = implementation.evaluate(state, logger)
    return {name if group is None else group}_metrics

def hpo(exp: {import_name}Info, data_root: str, run: list[str], branch: str, seed: int):
    implementation = import_module({module_string})
    storage_loc = {storage_string}
    os.makedirs(os.path.dirname(storage_loc), 777, exist_ok=True)
    study_name = {"f'{branch}'"}
    return implementation.prepare_hpo(exp, data_root, run[0], study_name, storage_loc, seed)

def test(exp: {import_name}Info, data_root: str, run: list[str], branch: str): pass
"""

    with open(f"{location}.py", "w") as fw:
        fw.write(file_string)
    return None

def write_configtype(name: str, group: str):
    import_name = ''.join([c.upper() if i==0 else c for i,c in enumerate(name)])
    inheritance = "Experiment" if group is None else "ZSLMUExperiment"
    file_string = f"""
@dataclass
class {import_name}Info({inheritance}Info):
    pass
"""
    with open(os.path.join(project_root, "src", "utils", "ConfigTypes.py"), "a") as fa:
        fa.write(file_string)
    
    return None

def write_source(name: str, group: str):
    import_name = ''.join([c.upper() if i==0 else c for i,c in enumerate(name)])
    study_string = """study_name=f\"{study_name}\", storage=f\"sqlite:///{storage_loc}\", load_if_exists=True"""
    file_string = f"""import os, sys, optuna
from dataclasses import dataclass
from torch.nn import Module as TorchModule
from torch import optim, no_grad
from torch.utils.data import DataLoader
from tqdm import tqdm

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."{', ".."' if group is not None else ''})))
from src.utils.ConfigTypes import {import_name}Info
from src.utils.addons import DynamicLogger
from src.utils.Interfaces import EvaluationBase

@dataclass
class TrainState():
    trainmodule: TorchModule = None
    trainloader: DataLoader = None
    validloader: DataLoader = None
    optimizer: optim.Optimizer = None
    nepochs: int = 0
    device: str = "cpu"

@dataclass
class EvalState():
    evalmodule: TorchModule = None
    testloader: DataLoader = None
    metrics: EvaluationBase = None
    device: str = "cpu"

@dataclass
class HPOState(): pass

# preparation functions #
def prepare_train(exp: {import_name}Info, dr: str, sn: str) -> TrainState:
    return TrainState()

def prepare_evaluate(exp: {import_name}Info, in_module: TorchModule, dr: str, sn: str, replace: bool) -> EvalState:
    return EvalState(evalmodule=in_module.to(exp.device))

def prepare_hpo(exp: {import_name}Info, dr: str, sn: str, study_name: str, storage_loc: str, seed: int) -> optuna.Study:
    already_exists = False
    if os.path.exists(storage_loc): already_exists = True 
    study = optuna.create_study({study_string})
    if already_exists: return hpo_analysis(study) 
    study.optimize(lambda trial: objective(HPOState(), trial), n_trials={"exp.hpo.ntrials" if group is None else "exp.method.hpo.ntrials"})
    study.set_user_attr("study_seed", seed)
    return study

# core functions #
@no_grad()
def validate(state: TrainState, epoch: int) -> bool:
    state.trainmodule.eval()
    return state.monitor.new_metric(0.0, silent=False, strict=True)

def train(state: TrainState, logger: DynamicLogger, desc: str = "Train") -> TorchModule:
    return state.trainmodule.to("cpu")

@no_grad()
def evaluate(state: EvalState, logger: DynamicLogger, desc: str = "Evaluate") -> EvaluationBase:
    return state.metrics

def objective(state: HPOState, trial: optuna.Trial): pass
def hpo_analysis(study: optuna.Study) -> optuna.Study: return study
"""

    with open(f"{impl_loc}.py", "w") as fw:
        fw.write(file_string)
    return None

def write_config(name: str, group: str):
    file_string = f"""name: {name}
origin: tunseen
forget: bforget
hyperparameters:
    max_epochs: 140
    patience: 10
    batch_size: 16
    optimizer:
        name: AdamW
        params:
            lr: 5e-6
hpo:
    ntrials: 100
"""
    with open(f"{conf_loc}.yaml", "w") as fw:
        fw.write(file_string)
    return None

## DELETION TASKS ##

def delete_experiment(name: str, group: str):
    os.remove(f"{location}.py")
def delete_source(name: str, group: str):
    os.remove(f"{impl_loc}.py")
    if group is not None and len([f for f in os.listdir(os.path.dirname(impl_loc)) if not f.startswith("__")]) == 0:
        os.remove(f"{location}.py")
def delete_config(name: str, group: str):
    os.remove(f"{conf_loc}.yaml")
def delete_configtype(name: str, group: str):
    name = ''.join([c.upper() if i == 0 else c for i,c in enumerate(name)])
    lines_to_copy: list[str] = []
    with open(os.path.join(project_root, "src", "utils", "ConfigTypes.py"), "r") as fr:
        ignore = False
        for line in fr.readlines():
            if "class" in line and f"{name}Info" in line and not ignore: 
                lines_to_copy.pop()
                ignore = True; 
                continue
            if ignore and "class" in line and not "__" in line: ignore = False
            if ignore: continue
            lines_to_copy.append(line)
    
    line = ""; original_line = ""
    while(line == ""):
        original_line = lines_to_copy.pop()
        line = original_line.replace(' ', '').replace('\n','')
    lines_to_copy.append(original_line)
    with open(os.path.join(project_root, "src", "utils", "ConfigTypes.py"), "w") as fw:
        fw.writelines(lines_to_copy)

## MAIN ##

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    allowed_groups = ["unlearn", "zslearn", "hycus", "mias"]
    parser.add_argument("-n", "--name", required=True, type=str, help="name of the experiment")
    parser.add_argument('-g', '--group', type=str, choices=allowed_groups, help="group where the experiment belongs")
    parser.add_argument('-d', '--delete', action='store_true', default=False, help="deletes the experiment")
    args = vars(parser.parse_args())
    
    project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
    
    location = os.path.join(project_root, "experiments", args["name"] if args["group"] is None else args["group"])
    
    impl_loc = os.path.join(project_root, "src", "implementations")
    if args["group"] is not None: impl_loc = os.path.join(impl_loc, args["group"])
    impl_loc = os.path.join(impl_loc, args["name"])

    conf_loc = os.path.join(project_root, "config", "experiment", "method", args["name"])
    
    assert not os.path.exists(impl_loc), f"implementation {impl_loc}.py already exists"
    
    if not args["delete"]: # Creation Task
        if not os.path.exists(f"{location}.py"): write_experiment(args["name"], args["group"])
        write_configtype(args["name"], args["group"])
        if args["group"] is not None: os.makedirs(os.path.dirname(impl_loc), mode=777, exist_ok=True)
        write_source(args["name"], args["group"])
        if args["group"]: write_config(args["name"], args["group"])
    
    else: # Deletion Task
        if args["group"] is None: delete_experiment(args["name"], args["group"])
        delete_configtype(args["name"], args["group"])
        delete_source(args["name"], args["group"])
        if args["group"]: delete_config(args["name"], args["group"])