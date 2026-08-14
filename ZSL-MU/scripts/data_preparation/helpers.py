import os, shutil
import numpy as np
from torchvision.datasets.utils import download_url
from pandas import read_csv
from glob import glob
from math import isclose
from zipfile import ZipFile
import wikipedia
import re

### DOWNLOADER ###
def download_archives(dsinfo, archive_loc: str, verbose: bool = False, forced: bool = False):
    # data archive
    darch_loc = os.path.join(archive_loc, f"{dsinfo.name}.{dsinfo.archformat}")
    if forced and os.path.exists(darch_loc): os.remove(darch_loc)
    if not os.path.exists(darch_loc):
        if verbose: print(f"downloading {dsinfo.name} data archive into {darch_loc}")
        download_url(dsinfo.source, archive_loc, f"{dsinfo.name}.{dsinfo.archformat}", md5=dsinfo.md5 if "md5" in dsinfo else None)

    # attributes archive
    aarch_loc = os.path.join(archive_loc, f"{dsinfo.name}_attrib.{dsinfo.attarchformat}") if not dsinfo.sameattribarch else os.path.join(archive_loc, f"{dsinfo.name}.{dsinfo.archformat}")
    if forced and not dsinfo.sameattribarch and os.path.exists(aarch_loc): os.remove(aarch_loc)
    if not os.path.exists(aarch_loc): 
        if verbose: print(f"downloading {dsinfo.name} attributes archive into {aarch_loc}")
        download_url(dsinfo.attsource, archive_loc, f"{dsinfo.name}_attrib.{dsinfo.attarchformat}")
    
    # splits archive
    if not os.path.exists(os.path.join(archive_loc, "splits.zip")): 
        if verbose: print(f"downloading splits archive into {os.path.join(archive_loc, 'splits.zip')}")
        download_url("https://cvml.ista.ac.at/AwA2/xlsa17.zip", archive_loc, "splits.zip")

def extract_unseen_splits(aloc: str, dloc: str, sloc: str, verbose: bool = False, forced: bool = False):
    splits_dst = os.path.join(dloc, "class_splits")
    if os.path.exists(splits_dst) and forced: shutil.rmtree(splits_dst)
    if os.path.exists(splits_dst): return None
    if verbose: print(f"Extracting splits into {splits_dst}")
    os.makedirs(splits_dst, 777)
    with ZipFile(os.path.join(aloc, "splits.zip")) as arch_ref:
        files = [f for f in arch_ref.namelist() if f.startswith(sloc) and f.endswith(".txt")]
        for f in files:
            file_name = translated_name(f)
            with arch_ref.open(f, "r") as fr, open(os.path.join(splits_dst, file_name), "wb") as fw:
                fw.write(fr.read())

def translated_name(split_file_name: str):
    '''
        translates xlsa17.zip files: 
        
            - testclasses -> unseenclasses
            - trainvalclasses -> seenclasses
            - valclasses -> training_unseenclasses (t_unseenclasses abbreviated)
            - trainclasses -> t_seenclasses
    '''
    translated_filename = split_file_name.split("/")[-1]
    translated_filename = translated_filename.replace("testclasses", "unseenclasses")
    translated_filename = translated_filename.replace("trainvalclasses", "seenclasses")
    translated_filename = translated_filename.replace("valclasses", "t_unseenclasses")
    translated_filename = translated_filename.replace("trainclasses", "t_seenclasses")
    return translated_filename

### SPLITS COMPILATION AND HELPERS ###
def compile_unseen_splits(dsname: str, data_root: str, verbose: bool = False):
    dataset_root = os.path.join(data_root, f"{dsname.upper()}_Data")
    class_maps = read_csv(os.path.join(dataset_root, "classes.csv"), sep=";", names=["class_id", "class_name"])
    class_maps = {r["class_name"]: r["class_id"] for _,r in class_maps.iterrows()}
    if not os.path.exists(os.path.join(dataset_root, "class_splits", "tunseen.txt")):
        if verbose: print(f"Compiling {dsname} test unseen file")
        unseen_classes = []
        with open(os.path.join(dataset_root, "class_splits", "unseenclasses.txt"), "r") as fr:
            unseen_classes.extend([l.replace("\n","") for l in fr.readlines()])
        unseen_classes = [class_maps[ucname] for ucname in unseen_classes]
        if verbose: print(f"{dsname} test unseen class ids: {unseen_classes}")
        with open(os.path.join(dataset_root, "class_splits", "tunseen.txt"), "w") as fw:
            fw.write(';'.join([str(cid) for cid in unseen_classes]))

    valid_unseen_versions = glob(os.path.join(dataset_root, "class_splits", "t_unseenclasses*.txt"))
    for vuv in valid_unseen_versions:
        version_number = int(os.path.split(vuv)[-1].split("t_unseenclasses")[1].split(".")[0])
        fname = f"vunseen{version_number}.txt"
        if not os.path.exists(os.path.join(dataset_root, "class_splits", fname)):
            if verbose: print(f"Compiling {dsname} validation unseen v{version_number} file")
            unseen_classes = []
            with open(vuv, "r") as fr:
                unseen_classes.extend([l.replace("\n","") for l in fr.readlines()])
            unseen_classes = [class_maps[ucname] for ucname in unseen_classes]
            if verbose: print(f"{dsname} validation unseen v{version_number} class ids: {unseen_classes}")
            with open(os.path.join(dataset_root, "class_splits", fname), "w") as fw:
                fw.write(';'.join([str(cid) for cid in unseen_classes]))
    
    if verbose: print("Deleting unnecessary files")
    [os.remove(f) for f in glob(os.path.join(dataset_root, "class_splits", "*.txt")) if "classes" in f]
    return

def get_splits(n: int, splits_amount: list[float], random: bool = False) -> list[list[int]]:
    '''
        gets a vector v and splits it into len(splits_amount) sub_vectors each of splits_amount[i]% indexes
        
        @Args:
            - n: number of elements in the list to be split into len(splits_amount splits)
            - splits_amount: vector of positive floats that have to sum up to 1
            - random: if set to True generate random splits
        
        @Returns:
            a list of len(splits_amount) splits where each element splits[i][j] is an index of v vector
    '''
    assert n > len(splits_amount), f"can't split a list of {n} elements into {len(splits_amount)} subsets"
    assert isclose(sum(splits_amount),1,rel_tol=1e-9), f"Invalid splits amount, they have to sum to 1 but they sum to {sum(splits_amount)}"
    assert all([s>=0 for s in splits_amount]), f"Invalid splits amount, they have to be positive but got {splits_amount}"

    indexes = list(range(n))
    if random:
        np.random.shuffle(indexes)        
    split_start = 0
    r = 0
    splits = [[] for _ in range(len(splits_amount))]
    for i, s in enumerate(splits_amount):
        float_split = s*n + r # let's say 312.6
        n_split = round(float_split) # 313
        r = float_split - n_split # -0.4 to be added to next float_split
        split_end = n_split+split_start 
        # [:] notation ignores last element but i have to take 313 elements for that split
        splits[i] = indexes[split_start:split_end+1]
        # positioning to split_end+1 hence 314th element
        split_start = split_end+1
    
    return splits

def compile_image_splits(rd: str, od: str, sd: str, splits: dict[str, float], forced: bool, verbose: bool):
    outdir = os.path.join(rd, od)
    if sd is not None: outdir = os.path.join(outdir, sd)
    os.makedirs(outdir, 777, exist_ok=True)
    images = read_csv(os.path.join(rd, "images.csv"), sep=";", names=["cid", "pth"]).groupby("cid")["pth"].apply(list).to_dict()
    total_images = sum([len(images[k]) for k in images.keys()])
    if verbose: print(f"found {total_images} images to split")
    sns = list(splits.keys())
    svs = list(splits.values())
    lines = {sn: [] for sn in sns}
    for cid,paths in images.items():
        if verbose: f"splitting {cid} class"
        ss = get_splits(len(paths), svs, random=True)
        for i,s in enumerate(ss):
            lines[sns[i]].extend([f"{cid};{pth}\n" for j,pth in enumerate(paths) if j in s])
    
    for sn in sns:
        outfile = os.path.join(outdir, f"{sn}.csv")
        if os.path.exists(outfile) and not forced: 
            print(f"{outfile} already found, skipping file")
            continue
        if os.path.exists(outfile): os.remove(outfile)
        if verbose: print(f"generating {outfile}.csv")
        with open(outfile, "w") as of:
            of.writelines(lines[sn])
    if verbose: print(f"validating generated splits")

    validate_image_splits(outdir, sns, total_images)
    return

def validate_image_splits(split_folder: str, split_names: list[str], total: int):
    # all files exist
    for sn in split_names:
        outfile = os.path.join(split_folder, f"{sn}.csv")
        assert os.path.exists(outfile), f"failed generating {outfile}"

    # sum of files coherent with total
    ofslines = [[] for _ in range(len(split_names))]
    for i in range(len(split_names)):
        outfile = os.path.join(split_folder, f"{split_names[i]}.csv")
        with open(outfile, "r") as of:
            ofslines[i].extend([l for l in of.readlines()])
    found_images = sum([len(ofl) for ofl in ofslines])
    assert found_images == total, f"Expected {total} lines but splits only have: {found_images}"

    # no overlaps
    for i in range(len(ofslines)):
        for j in range(i+1, len(ofslines)):
            overlaps = len([img for img in ofslines[i] if img in ofslines[j]])
            assert overlaps==0, f"found {overlaps} between {split_names[i]} and {split_names[j]}"

    return None

def compile_forget_set(data_root: str, name: str, nforget: int, from_files: str = None, unseen_file: str = None, forced: bool = False):
    choices = []
    unseen = load_classes(os.path.join(data_root, "class_splits", f"{unseen_file}.txt")) if unseen_file is not None else []
    
    for ff in from_files:
        choices.extend(load_classes(os.path.join(data_root, "class_splits", f"{ff}.txt")))
    choices = list(set(choices))

    cids = list(set([r["cid"] for _, r in read_csv(os.path.join(data_root, "classes.csv"), sep=";", names=["cid", "cname"]).iterrows() if r["cid"] not in unseen]))
    if len(choices) > 0: cids = [cid for cid in cids if cid in choices]

    outfile = os.path.join(data_root, "class_splits", f"{name}.txt")
    if os.path.exists(outfile) and not forced: assert False, f"{outfile} already exists, use --forced to replace it"
    if os.path.exists(outfile): os.remove(outfile)
    
    return save_classes(outfile, np.random.choice(cids, nforget, replace=False).tolist())

def get_wiki_description(class_name: str, language: str = "en"):
    wikipedia.set_lang(language)
    search_results = wikipedia.search(class_name)
    if not search_results: return None
    to_classname = lambda sr: re.sub(r"'s\b", "", sr.strip().replace("-", " ").replace("_", " ").lower())

    try:
        assert isinstance(search_results[0], str), f"expected string but got {type(search_results[0])}"
        search_result: str = to_classname(search_results[0])
        assert search_result == class_name.lower(), f"class name and page missmatch: {class_name} != {search_result}"
        
        page = wikipedia.page(search_results[0])
        print(f"class {class_name} - Succeeded on page {search_results[0]}")
        return page.summary.strip()
    except (wikipedia.exceptions.DisambiguationError, wikipedia.exceptions.PageError) as err:
        print(f"Wikipedia error on class {class_name}, page {search_results[0]} - using class name as descriptor")
        return None
    except Exception as e:
        print(f"Generic error on class {class_name}, page {search_results[0]} - using class name as descriptor")
        return None

def format_classname(class_list: list[str], dataset: str):
    available_datasets = ["cub", "awa2", "apy", "sun"]
    assert dataset in available_datasets, f"{dataset} not available, add implementation before using it"
    if dataset == "apy": return class_list
    if dataset == "cub": return [" ".join(cn.split(".")[1].lower().split("_")) for cn in class_list]
    if dataset == "sun": return [" ".join(cn.lower().split("_")) for cn in class_list]
    if dataset == "awa2": return [" ".join(cn.lower().split("+")) for cn in class_list]

def load_classes(file: str) -> list[int]:
    loaded_list = []
    with open(file, "r") as fr:
        loaded_list = sorted([int(cid) for cid in fr.read().split(';')])
    return loaded_list

def save_classes(file: str, cids: list[int]) -> None:
    with open(file, "w") as fw:
        fw.write(';'.join([str(cid) for cid in cids]))
    return None
