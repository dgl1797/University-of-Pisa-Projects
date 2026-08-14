import os,sys
from omegaconf import OmegaConf
import torch

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from src.utils.ConfigTypes import ClassifierInfo
from src.models import ClassifierModel, HyCUSBase

if __name__ == '__main__':
    print("allocated before init: ", torch.cuda.max_memory_allocated(device=None))
    classifierinfo: ClassifierInfo = OmegaConf.load(os.path.join("config", "experiment", "classifier", "resnet18.yaml"))
    classifier = ClassifierModel(classifierinfo, 150, True)
    
    print("allocated after classifier init: ", torch.cuda.max_memory_allocated(device=None))
    print(sum([p.numel() for p in classifier.model.layer4[1].conv1.parameters() if p.requires_grad]))
    print(classifier.model.layer4[1].conv1.weight.flatten().shape)

    hycus = HyCUSBase(classifier.model.layer4[1].conv1.weight.flatten().shape[0], 25088, 512, 0.0).to("cuda")
    print(sum([p.numel() for p in hycus.parameters() if p.requires_grad]))
    print("allocated after hycus init: ", torch.cuda.max_memory_allocated(device=None))