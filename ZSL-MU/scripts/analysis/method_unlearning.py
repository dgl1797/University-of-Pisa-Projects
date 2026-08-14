import os,sys
from omegaconf import OmegaConf
import torch
from pandas import read_csv
from matplotlib import pyplot as plt
import argparse
from numpy import load as npyload

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.utils.ConfigTypes import ClassifierInfo
from src.datasets.loaders import load_txt
from src.models import ClassifierModel, HyCUSBase
from src.metrics import HyCUSEval    

def plot_and_save(hycusmetrics: HyCUSEval, method: str, dataset: str, forget: str):
    allmetrics = {k:v[0] for k,v in hycusmetrics.todict().items()}
    nrows = 3; ncols = 3
    _, axes = plt.subplots(nrows, ncols)

    # shared histograms
    axes[0][0].hist(allmetrics["shared_unlearn"], bins=64, edgecolor='black', alpha=0.7, color='lightgrey')
    axes[0][0].set_title("shared_unlearn")
    axes[0][1].hist(allmetrics["shared_origin"], bins=64, edgecolor='black', alpha=0.7, color='lightgrey')
    axes[0][1].set_title("shared_origin")
    axes[0][2].hist(allmetrics["shared_golden"], bins=64, edgecolor='black', alpha=0.7, color='lightgrey')
    axes[0][2].set_title("shared_golden")

    # distinct retain histograms
    axes[1][0].hist(allmetrics["distinct_retain_unlearn"], bins=64, edgecolor="black", alpha=0.7, color="lightgreen")
    axes[1][0].set_title("distinct_retain_unlearn")
    axes[1][1].hist(allmetrics["distinct_retain_origin"], bins=64, edgecolor="black", alpha=0.7, color="lightgreen")
    axes[1][1].set_title("distinct_retain_origin")
    axes[1][2].hist(allmetrics["distinct_retain_golden"], bins=64, edgecolor="black", alpha=0.7, color="lightgreen")
    axes[1][2].set_title("distinct_retain_golden")

    # distinct forget histograms
    axes[2][0].hist(allmetrics["distinct_forget_unlearn"], bins=64, edgecolor="black", alpha=0.7, color="lightcoral")
    axes[2][0].set_title("distinct_forget_unlearn")
    axes[2][1].hist(allmetrics["distinct_forget_origin"], bins=64, edgecolor="black", alpha=0.7, color="lightcoral")
    axes[2][1].set_title("distinct_forget_origin")
    axes[2][2].hist(allmetrics["distinct_forget_golden"], bins=64, edgecolor="black", alpha=0.7, color="lightcoral")
    axes[2][2].set_title("distinct_forget_golden")

    # show
    saveloc = os.path.join("out", "plottings", "base", "unlearn", f"{method}_tunseen", f"{forget}_resnet18_{dataset}.png")
    os.makedirs(os.path.dirname(saveloc), 777, exist_ok=True)
    if os.path.exists(saveloc): os.remove(saveloc)
    plt.tight_layout(); plt.savefig(saveloc, transparent=True); plt.show()
    return None

def run_analysis(dataset: str, forget: str, method: str):
    print(f"Analyzing {dataset} - {forget}")
    classifierinfo: ClassifierInfo = OmegaConf.load(os.path.join("config", "experiment", "classifier", "resnet18.yaml"))
    forgetloc = os.path.join("data", f"{dataset.upper()}_Data", "class_splits", f"{forget}.txt")
    unseenloc = os.path.join("data", f"{dataset.upper()}_Data", "class_splits", "tunseen.txt")
    allcidloc = os.path.join("data", f"{dataset.upper()}_Data", "classes.csv")
    forgetset = load_txt(forgetloc)
    unseenset = load_txt(unseenloc)
    allcidset = [r["cid"] for _,r in read_csv(allcidloc, sep=";", names=["cid", "name"]).iterrows()]
    seencids = sorted([cid for cid in allcidset if cid not in unseenset])
    nclasses = len(seencids)

    origin = ClassifierModel(classifierinfo, nclasses, False).eval()
    golden = ClassifierModel(classifierinfo, nclasses, False).eval()
    unlearned = ClassifierModel(classifierinfo, nclasses, False).eval()

    originloc = os.path.join("out", "checkpoints", "base", "origin", "tunseen", f"resnet18_{dataset}.pt")
    goldenloc = os.path.join("out", "checkpoints", "base", "unlearn", "retrain_tunseen", f"{forget}_resnet18_{dataset}.pt")
    unlearnedloc = os.path.join("out", "checkpoints", "base", "unlearn", f"{method}_tunseen", f"{forget}_resnet18_{dataset}.pt")

    origin.load_state_dict(torch.load(originloc))
    golden.load_state_dict(torch.load(goldenloc))
    unlearned.load_state_dict(torch.load(unlearnedloc))

    gwd = unlearned.get_classifier_weights(); gws = unlearned.get_backbone_weights(["layer4.1.bn2"]).unsqueeze(0).repeat(gwd.shape[0], 1)
    owd = origin.get_classifier_weights(); ows = origin.get_backbone_weights(["layer4.1.bn2"])
    rwd = golden.get_classifier_weights(); rws = golden.get_backbone_weights(["layer4.1.bn2"])

    translated_forgetset = [seencids.index(fcid) for fcid in forgetset]
    hycusmetrics = HyCUSEval(gws, gwd, ows, owd, rws, rwd, torch.randn(nclasses, 312), torch.randn(nclasses, 312), translated_forgetset)

    return plot_and_save(hycusmetrics, method, dataset, forget)

def hycus_analysis(dataset: str, forget: str, method: str):
    print(f"Analyzing {dataset} - {forget}")
    classifierinfo: ClassifierInfo = OmegaConf.load(os.path.join("config", "experiment", "classifier", "resnet18.yaml"))
    forgetloc = os.path.join("data", f"{dataset.upper()}_Data", "class_splits", f"{forget}.txt")
    unseenloc = os.path.join("data", f"{dataset.upper()}_Data", "class_splits", "tunseen.txt")
    semantloc = os.path.join("data", f"{dataset.upper()}_Data", "embeddings", "attribs.npy")
    allcidloc = os.path.join("data", f"{dataset.upper()}_Data", "classes.csv")
    forgetset = load_txt(forgetloc)
    unseenset = load_txt(unseenloc)
    allcidset = sorted([r["cid"] for _,r in read_csv(allcidloc, sep=";", names=["cid", "name"]).iterrows()])
    seencids = sorted([cid for cid in allcidset if cid not in unseenset])
    nclasses = len(seencids)
    semantics = torch.tensor(npyload(semantloc).T[[allcidset.index(scid) for scid in seencids], :])

    origin = ClassifierModel(classifierinfo, nclasses, False).eval()
    golden = ClassifierModel(classifierinfo, nclasses, False).eval()
    unlearned = ClassifierModel(classifierinfo, nclasses, False).eval()

    originloc = os.path.join("out", "checkpoints", "base", "origin", "tunseen", f"resnet18_{dataset}.pt")
    goldenloc = os.path.join("out", "checkpoints", "base", "unlearn", "retrain_tunseen", f"{forget}_resnet18_{dataset}.pt")
    hycusloc = os.path.join("out", "checkpoints", "base", "hycus", f"{method}_tunseen", f"{forget}_attribs_resnet18_{dataset}.pt")

    origin.load_state_dict(torch.load(originloc))
    golden.load_state_dict(torch.load(goldenloc))
    unlearned.load_state_dict(torch.load(originloc))

    owd = origin.get_classifier_weights(); ows = origin.get_backbone_weights(["layer4.1.bn2"])
    rwd = golden.get_classifier_weights(); rws = golden.get_backbone_weights(["layer4.1.bn2"])

    hycus = HyCUSBase(ows.shape[0]+owd.shape[1], semantics.shape[1], 512, 0.0).eval()
    hycus.load_state_dict(torch.load(hycusloc))
    gws, gwd = hycus.make_weights(torch.cat((ows.unsqueeze(0).repeat(owd.shape[0], 1), owd), dim=1), ows.shape[0])

    unlearned.model.layer4[1].bn2.weight.data = gws[:gws.shape[0]//2]
    unlearned.model.layer4[1].bn2.bias.data = gws[gws.shape[0]//2:gws.shape[0]]
    unlearned.model.fc.weight.data = gwd[:, :-1]
    unlearned.model.fc.bias.data = gwd[:, -1]

    gwd = unlearned.get_classifier_weights(); gws = unlearned.get_backbone_weights(["layer4.1.bn2"]).unsqueeze(0).repeat(gwd.shape[0], 1)

    translated_forgetset = [seencids.index(fcid) for fcid in forgetset]
    hycusmetrics = HyCUSEval(gws, gwd, ows, owd, rws, rwd, torch.randn(nclasses, 312), torch.randn(nclasses, 312), translated_forgetset)

    return plot_and_save(hycusmetrics, "hycus", dataset, forget)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--datasets", type=str, nargs='+', choices=["cub", "awa2", "apy"], required=True, help="datasets to be analyzed")
    parser.add_argument("-m", "--method", type=str, default="hycus", choices=["hycus", "scrub", "badt", "neggradplus"], help="the unlearned model to analyze")
    args = vars(parser.parse_args())

    forget_list: list[str] = ["lforget", "bforget", "hforget"]
    dataset_list: list[str] = args["datasets"]
    method: str = args["method"]

    if method != "hycus": [run_analysis(ds, fs, method) for ds in dataset_list for fs in forget_list]
    else: [hycus_analysis(ds, fs, "classic") for ds in dataset_list for fs in forget_list]