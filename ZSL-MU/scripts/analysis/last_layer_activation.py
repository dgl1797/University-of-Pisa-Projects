import sys, os, argparse
from omegaconf import OmegaConf
from torchvision.transforms import Compose, Resize, ToTensor
from torch import load as torch_load
import numpy as np
import matplotlib.pyplot as plt
from tqdm import tqdm

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.models import ClassifierModel
from src.utils.ConfigTypes import DatasetInfo, ClassifierInfo
from src.datasets.loaders import load_classification_sets
from scripts.analysis import plotters

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--dataset", type=str, choices=["cub", "apy", "awa2"], required=True, help="of which dataset to see the activations at layer4.1.relu")
    parser.add_argument("-m", "--model", type=str, default="resnet18", help="sets the model to be checked - default is resnet18")
    parser.add_argument("-c", "--cids", type=int, nargs="+", default=[], help="cids of which to see the activations")
    parser.add_argument("-f", "--forget", type=str, choices=["lforget", "bforget", "hforget"], default="bforget", help="sets the forgetset for the golden model - default is bforget")
    parser.add_argument("-s", "--split", type=str, default="base", help="the image split to be used to load image - default is base")
    parser.add_argument("-r", "--root", type=str, default="data", help="set to change default data-root location (default is ./data)")

    args = vars(parser.parse_args())
    dr: str = args["root"]
    model_name: str = args["model"]
    forget: str = args["forget"]
    dataset_name: str = args["dataset"]
    img_split: str = args["split"]
    check_cids: list[int] = sorted(args["cids"])

    dsinfo: DatasetInfo = OmegaConf.load(os.path.join("config", "experiment", "dataset", f"{dataset_name}.yaml"))
    csinfo: ClassifierInfo = OmegaConf.load(os.path.join("config", "experiment", "classifier", f"{model_name}.yaml"))
    nclasses = dsinfo.ntotal_class - dsinfo.nunseen_test

    # dataset and classifier retrieval
    split_loc = os.path.join(dr, f"{dataset_name.upper()}_Data", "image_splits", img_split)
    unseenloc = os.path.join(dr, f"{dataset_name.upper()}_Data", "class_splits", "tunseen.txt")
    origin_name = f"{model_name}_{dataset_name}.pt"
    golden_name = f"{forget}_{origin_name}"
    originloc = os.path.join("out", "checkpoints", img_split, "origin", "tunseen", origin_name)
    goldenloc = os.path.join("out", "checkpoints", img_split, "unlearn", "retrain_tunseen", golden_name)
    transform = Compose([Resize((csinfo.input_size, csinfo.input_size)), ToTensor()])
    trainds, testds, validds = load_classification_sets(split_loc, unseenloc, transform)
    origin = ClassifierModel(csinfo, nclasses, False)
    origin.load_state_dict(torch_load(originloc))
    origin.eval().to("cuda")
    golden = ClassifierModel(csinfo, nclasses, False)
    golden.load_state_dict(torch_load(goldenloc))
    golden.eval().to("cuda")

    perclass_indexes: list[list[int]] = []; prev_cid = -1; cids_count = -1 # loop initialization state
    for index,(cid,_) in enumerate(trainds):
        if cid != prev_cid: perclass_indexes.append([]); cids_count += 1; prev_cid = cid
        perclass_indexes[cids_count].append(index)

    origin_one_shot_activations = np.empty((nclasses, 25088), dtype=np.float_) # computed on a single selection selected randomly
    origin_full_shot_activations = np.empty((nclasses, 25088), dtype=np.float_) # computed on all class' samples by averaging
    golden_one_shot_activations = np.empty((nclasses, 25088), dtype=np.float_)
    golden_full_shot_activations = np.empty((nclasses, 25088), dtype=np.float_)

    translated_check_cids = [trainds.cids_map.index(cc) for cc in check_cids] if len(check_cids) > 0 else None

    print("computing one-shot activations")
    for i in (translated_check_cids if translated_check_cids is not None else range(nclasses)):
        random_class_index = np.random.choice(perclass_indexes[i])
        sample_x, _ = trainds[random_class_index]
        origin_activations = origin.extract_output_from(sample_x.to("cuda").unsqueeze(0), "layer4.1.relu").flatten()
        golden_activations = golden.extract_output_from(sample_x.to("cuda").unsqueeze(0), "layer4.1.relu").flatten()
        origin_one_shot_activations[i, :] = origin_activations.to("cpu").numpy()[:]
        golden_one_shot_activations[i, :] = golden_activations.to("cpu").numpy()[:]
    
    for i in tqdm(translated_check_cids if translated_check_cids is not None else range(nclasses), desc="computing full_shot_activations"):
        origin_tot_activations = np.empty((len(perclass_indexes[i]), 25088), dtype=np.float_)
        golden_tot_activations = np.empty((len(perclass_indexes[i]), 25088), dtype=np.float_)
        for sample_index in perclass_indexes[i]:
            sample_x, _ = trainds[sample_index]
            origin_sample_activations = origin.extract_output_from(sample_x.to("cuda").unsqueeze(0), "layer4.1.relu").flatten()
            golden_sample_activations = golden.extract_output_from(sample_x.to("cuda").unsqueeze(0), "layer4.1.relu").flatten()
            origin_tot_activations = np.concatenate((origin_tot_activations, origin_sample_activations.to("cpu").unsqueeze(0).numpy()), axis=0)
            golden_tot_activations = np.concatenate((golden_tot_activations, golden_sample_activations.to("cpu").unsqueeze(0).numpy()), axis=0)
        
        origin_tot_activations: np.ndarray = origin_tot_activations.mean(axis=0)
        golden_tot_activations: np.ndarray = golden_tot_activations.mean(axis=0)
        origin_full_shot_activations[i, :] = origin_tot_activations[:]
        golden_full_shot_activations[i, :] = golden_tot_activations[:]

    # saving transposed array as other embeddings
    np.save("osactiv.npy", origin_one_shot_activations.T)
    np.save("fsactiv.npy", origin_full_shot_activations.T)
    
    # plottings
    plotters.plot_heatmaps(origin_one_shot_activations, golden_one_shot_activations, origin_full_shot_activations, golden_full_shot_activations, translated_check_cids, check_cids)
    plotters.plot_boxplots(origin_one_shot_activations, golden_one_shot_activations, origin_full_shot_activations, golden_full_shot_activations, translated_check_cids, check_cids)
