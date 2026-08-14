from omegaconf import OmegaConf
from pathlib import Path
from dataclasses import dataclass
from typing import Any
from itertools import product
from argparse import ArgumentParser

def unpack_multiruns(multiruns: dict[str, list[Any]]) -> list[dict[str, Any]]:
    single_runs = [[{k: vx} for vx in multiruns[k]] for k in multiruns.keys()]
    single_runs = [{k: v for p in r for k,v in p.items()} for r in list(product(*single_runs))]
    return single_runs

def pair(param: str):
    key, value = param.split("=", 1)
    return {key: value}

def multirun_pair(param: str):
    key, value = param.split("=", 1)
    values = value.replace("[", "").replace("]", "").split(",")
    return {key: values}

def use_config(conf_file: str, schema: dataclass = None, overrides: dict[str, Any] = {}, multiruns: dict[str, list[Any]] = {}, parse_args: bool = False):
    '''
    loads the configuration in ./config/conf_file using omegaconf allowing also overrides and schema validation. Allows also multirun configurations
    
    **Arguments**
        - *conf_file*: relative path from ./config to the .yaml file
        
        - *schema*: Dataclass containing the schema to be validated during loading
        
        - *overrides*: dictionary of key-value pairs where inner properties can be overridden by using dot-separated keys
        `-o key1=value1 key2=value2` for cli overriding

        - *multiruns*: dictionary of key-list_of_values formatted as overrides
        `-m key1=[value1,value2] key2=[value3,value4]` for cli multirun

        - *parse_args*: boolean flag to enable/disable argument parsing for cli override/multirun. When enabled cli parameters will
        have priority over coded parameters - default is False
    
    **Returns**
        the list of returned values from each execution of the wrapped function, if multirun is specified otherwise the result of the
        wrapped function 

    '''
    conf_baseloc = Path("config")
    if not conf_file.endswith(".yaml"): conf_file = f"{conf_file}.yaml"

    # argument_parsing
    if parse_args:
        parser = ArgumentParser()
        meg = parser.add_mutually_exclusive_group()
        meg.add_argument("-o", "--override", type=pair, nargs='+', required=False, help="series of key=value pairs to override arguments, dot-separated keys will be automatically mapped: training.batchsize=31 ae.nblocks=1")
        meg.add_argument("-m", "--multirun", type=multirun_pair, nargs='+', required=False, help="series of key=value1,value2 pairs, values must be comma-separated, dot-separated keys will be automatically mapped: training.batchsize=[12,24] ae.nblocks=1,2")
        args = vars(parser.parse_args())
        if "override" in args and args["override"] is not None: overrides = {k: v for p in args["override"] for k,v in p.items()}
        if "multirun" in args and args["multirun"] is not None: multiruns = {k: v for p in args["multirun"] for k,v in p.items()}
    
    def decorator(function):

        def wrapper(*args, **kwargs):
            config = OmegaConf.structured(schema) if schema is not None else OmegaConf.create({})
            confcontent = OmegaConf.load(conf_baseloc.joinpath(conf_file))
            if "import" in confcontent:
                for conf in confcontent["import"]:
                    # case is a list of imports -> just merge every import:
                    if OmegaConf.is_config(conf): name, conf = list(conf.items())[0]
                    if not conf.endswith(".yaml"): conf = f"{conf}.yaml"
                    conf = OmegaConf.load(conf_baseloc.joinpath(conf))
                    if name: confcontent[name] = OmegaConf.merge({}, conf)
                    else: confcontent.merge_with(conf)
                
                del confcontent["import"]
            config.merge_with(confcontent)
            
            # simple override
            if len(multiruns) == 0: 
                [OmegaConf.update(config, ovk, ovv) for ovk,ovv in overrides.items()]
                return function(config, *args, **kwargs)
            
            # multirun execution
            multirun_results = []
            runs = unpack_multiruns(multiruns); nruns = len(runs)
            for rdx, override in enumerate(runs):
                try:
                    print(f"\033[92mExecuting task {rdx+1}/{nruns}, with override: {override}\033[0m")
                    [OmegaConf.update(config, ovk, ovv) for ovk,ovv in override.items()]
                    this_result = function(config, *args, **kwargs)
                    multirun_results.append(this_result)
                except Exception as e:
                    print(f"\033[91mError during execution of run {rdx+1}\033[0m")
                    raise Exception(*e.args)
            return multirun_results
    
        return wrapper
    
    return decorator

'''
## EXAMPLE OF USAGE 1 - no schema and single override, not allowing argument-parsing
@use_config(conf_file="sane", schema=None, overrides={
    "training.batchsize": 47,
    "ae.nblocks": 12
})
def example_function1(config, name: str):
    print(f"---- My name is {name} ----")
    print(config)

## EXAMPLE OF USAGE 2 - schema with single override allowing for argument-parsing (cli-arguments will have priority)
from schemas.sane import SANE_CONF

@use_config(conf_file="sane", schema=SANE_CONF, parse_args=True, overrides={
    "training.batchsize": 47, "ae.nblocks": 12
})
def example_function2(config: SANE_CONF, name: str):
    print(f"---- My name is {name} ----")
    print(config)

## EXAMPLE OF USAGE 3 - schema with multirun overriding without argument-parsing
@use_config(conf_file="sane", schema=SANE_CONF, multiruns={
    "training.batchsize": [16,32], "ae.nblocks": [1,2,4]
})
def example_function3(config: SANE_CONF, name: str):
    print(f"---- My name is {name} ----")
    print(config)

## EXAMPLE OF USAGE 4 - no schema with multirun overriding allowing for argument-parsing (cli-arguments will have priority)
@use_config(conf_file="sane", overrides={
    "training.batchsize": 16 # will be ignored as multirun has priority
}, multiruns={
    "training.batchsize": [12,24,32],
    "ae.nblocks": [7,8]
})
def example_function4(config: SANE_CONF, name: str):
    print(f"---- My name is {name} ----")
    print(config)

if __name__ == "__main__":
    example_function1("f1")
    example_function2("f2")
    example_function3("f3")
    example_function4("f4")
'''