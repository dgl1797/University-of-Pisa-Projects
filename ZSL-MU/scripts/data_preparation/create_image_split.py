import argparse, os
from helpers import compile_image_splits

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="create new splits from images for a given dataset, storing in data_root/image_splits[/subdir]")
    parser.add_argument("-d", "--datasets", nargs='+', type=str, choices=["cub","awa2","sun","apy"], required=True, help="datasets to split")
    parser.add_argument("-k", "--keys", nargs='+', type=str, required=True, help="The names of the splits, at least 2 required")
    parser.add_argument("-v", "--values", nargs='+', type=float, required=True, help="The values for each split")
    parser.add_argument("-n", "--name", type=str, default="base", help="subdirectory for storing splits, works as name for the split")
    parser.add_argument("-f", "--forced", action='store_true', default=False, help="Force resplit if splits are already present")
    parser.add_argument("-r", "--root", type=str, default="data", help="the data root where to find images")
    parser.add_argument("--verbose", action='store_true', default=False)
    args = vars(parser.parse_args())
    assert len(args["keys"])>1, f"required at least 2 splitting keys, {len(args['keys'])} received"
    assert len(args["values"]) == len(args["keys"]), f"required same amount of keys and values, k:{len(args['keys'])} v:{len(args['values'])} received"
    splits = {k: v for k,v in zip(args["keys"], args["values"])}
    rd = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", args["root"]))
    assert os.path.exists(rd), f"path {rd} doesn't exist"
    sd = args["name"] if args["name"] is not None else "base"
    forced = args["forced"]
    verbose = args["verbose"]
    dss: list[str] = args["datasets"]
    for ds in dss:
        if verbose: print(f"Splitting dataset {ds} with splits: {splits}")
        dsdir = os.path.join(rd, f"{ds.upper()}_Data")
        compile_image_splits(dsdir, "image_splits", sd, splits, forced, verbose)