import torch, os, sys
from pathlib import Path
from matplotlib import pyplot as plt
import numpy as npy

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from src.datasets.images.generic_image import GenericImagesDataset
from src.datasets.images.helpers import load_rows_from_csv, load_txt
from src.implementations.classification import ClassificationTask
from src.models.classifiers.resnet18 import ResNet18

def layer_by_layer_analysis(reference: dict[str, torch.Tensor], comparison: dict[str, torch.Tensor]):
    assert all([k1 == k2 for k1,k2 in zip(reference.keys(), comparison.keys())]), "dictionaries do not refer to the same model"
    
    ldx = 0
    for key in reference.keys():
        assert reference[key].shape == comparison[key].shape, f"layer {ldx} - {key} missmatch"
        if "bias" in key: continue
        if "weight" not in key and "running_mean" not in key and "running_var" not in key: continue
        hasbias = False
        if "weight" in key and key.replace("weight", "bias") in reference:
            hasbias = True
            refb = reference[key.replace("weight", "bias")].flatten().numpy()
            cmpb = comparison[key.replace("weight", "bias")].flatten().numpy()
            ref = npy.concatenate((ref, refb)); cmp = npy.concatenate((cmp, cmpb))
        ref: npy.ndarray = reference[key].flatten().numpy(); cmp: npy.ndarray = comparison[key].flatten().numpy()
        x = npy.arange(ref.shape[0])
        fig = plt.figure(figsize=(12,8))
        fig.suptitle(f"Layer {ldx} - {key}{'.bias' if hasbias else ''}", fontsize=12)
        charts = fig.add_subplot(1,1,1)
        charts.plot(x, ref, color='lightgreen', label='reference', linewidth=0.7)
        charts.plot(x, cmp, color='lightcoral', label='reconstruct', linewidth=0.7, alpha=0.5)
        charts.set_xlabel('index'); charts.set_ylabel('value')
        charts.legend()
        plt.tight_layout()
        yield charts
        plt.close(fig)
        ldx+=1

def create_forward_recorder(name: str):
    def hook(m: torch.nn.Module, i: torch.Tensor, o: torch.Tensor):
        input_data = i[0]; output_data = o
        assert isinstance(input_data, torch.Tensor), f"input type: {type(input_data)} @ {name}, {input_data}"
        assert isinstance(o, torch.Tensor), f"output type: {type(o)} @ {name}"
        assert (input_data != 0).any().item(), f"layer {name} is receiving fully 0 input"
        assert (output_data != 0).any().item(), f"layer {name} is a dead layer"
        assert not input_data.isnan().any().item(), f"layer {name} received a nan input"
        assert not output_data.isnan().any().item(), f"layer {name} produced a nan output: running_var = {m.running_var}, running_mean = {m.running_mean}"
    return hook

def debug():
    csvloc = Path("data", "AWA2_Data", "image_splits", "base", "test.csv")
    fltloc = Path("data", "AWA2_Data", "class_splits", "tunseen.txt")
    dbgloc = Path("out", "sane_augment.stride_wsquarti", "injections", "injected.pt")
    orgloc = Path("checkpoints", "resnet18_awa2.pt")
    imgds = GenericImagesDataset(load_rows_from_csv(csvloc), load_txt(fltloc), transform="default")
    imageloader = torch.utils.data.DataLoader(imgds, 32, False, num_workers=4, persistent_workers=True)

    nclasses = len(imgds.cids_map)
    classifier = ResNet18(nclasses, False)
    task = ClassificationTask("debugging", nclasses, "cuda")
    original_checkpoint = torch.load(orgloc)
    reconstr_checkpoint = torch.load(dbgloc)


    classifier.load_state_dict(reconstr_checkpoint)
    for name, module in classifier.named_modules():
        if len(list(module.children())) > 0: continue
        module.register_forward_hook(create_forward_recorder(name))
    recon_metrics = task(classifier, imageloader, "Recon Eval")
    print(recon_metrics)

    # for image in layer_by_layer_analysis(reference=original_checkpoint, comparison=reconstr_checkpoint):
    #     plt.show()


if __name__ == "__main__":
    debug()