import argparse, os
from helpers import compile_forget_set

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--datasets", nargs='+', type=str, required=True, choices=["sun", "apy", "cub", "awa2"], help="the datasets of which to create the splits")
    parser.add_argument("-nf", "--n-forget", nargs='+', type=int, required=True, help="number of forget labels for each dataset to extract")
    parser.add_argument("-n", "--name", type=str, default="unlbase", help="name of the split")
    parser.add_argument("-u", "--unseen", type=str, help=".txt file of unseen labels to be filtered out before generating the forget set")
    parser.add_argument("--from", nargs='+', type=str, default=[], help=".txt files of labels from which to extract the forget set")
    parser.add_argument("-f", "--forced", action='store_true', default=False, help="forces rewrite of the split if already present")
    parser.add_argument("-r", "--root", type=str, default="data", help="data root directory, default is ./data")
    args = vars(parser.parse_args())

    assert len(args["datasets"]) == len(args["n_forget"]), f"incompatible number of datasets: {len(args['datasets'])} and n-forgets: {len(args['n_forget'])}"
    
    from_files = args["from"] if "from" in args else None
    unseen_file = args["unseen"] if "unseen" in args else None

    for idx, dsname in enumerate(args["datasets"]):
        root_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", *args["root"].split("/")))
        root_dir = os.path.join(root_dir, f"{dsname.upper()}_Data")
        nforgets = args["n_forget"][idx]
        compile_forget_set(root_dir, args["name"], nforgets, from_files, unseen_file, args["forced"])