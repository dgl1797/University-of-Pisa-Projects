import os, argparse, extractors, shutil
from omegaconf import OmegaConf
from helpers import download_archives, compile_unseen_splits, extract_unseen_splits

def download(dsname: str):
    config_l = os.path.join("config", "download_info", f"{dsname}.yaml")
    archiv_l = os.path.join(rootdir, "archives")
    dsinfo = OmegaConf.load(config_l)
    
    return download_archives(dsinfo, archiv_l, verbose, redownload)

def extract(dsname: str):
    destin_l = os.path.join(rootdir, f"{dsname.upper()}_Data")
    archiv_l = os.path.join(rootdir, "archives")
    splits_l = f"xlsa17/data/{dsname.upper()}"

    if forced and os.path.exists(destin_l): shutil.rmtree(destin_l)
    if not os.path.exists(destin_l):
        os.makedirs(destin_l, 777)
        # extract data, compiling classes.csv, images.csv and attribs.csv for dataset
        getattr(extractors, f"extract_{dsname}")(archiv_l, destin_l, verbose, forced)
        extract_unseen_splits(archiv_l, destin_l, splits_l, verbose, forced)
        compile_unseen_splits(dsname, rootdir, verbose)
    
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--datasets", nargs='+', type=str, choices=["cub","awa2","sun","apy"], required=True, help="datasets to split")
    parser.add_argument("-r", "--root", type=str, default="data", help="the data root where place files")
    parser.add_argument("--download", action='store_true', default=False, help="forces redownload of archives")
    parser.add_argument("-f", "--forced", action='store_true', default=False, help="forces re-extraction of files substituting if already exist")
    parser.add_argument("--verbose", action='store_true', default=False)
    args = vars(parser.parse_args())

    dsnames = args["datasets"]
    rootdir = os.path.join(*args["root"].split("/"))
    verbose = args["verbose"]
    forced = args["forced"]
    redownload = args["download"]

    for dsname in dsnames:
        if dsname == "apy": download("apascal"); download("ayahoo")
        else: download(dsname)
        extract(dsname)