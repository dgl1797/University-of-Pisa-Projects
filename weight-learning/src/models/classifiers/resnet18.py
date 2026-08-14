import torch
from torchvision.models import get_model, get_weight
from torchvision.models.resnet import ResNet18_Weights
from copy import deepcopy

class ResNet18(torch.nn.Module):
    def __init__(self, nclasses: int, finetune: bool = False):
        super(ResNet18, self).__init__()
        self.model = get_model("resnet18", weights=get_weight("ResNet18_Weights.IMAGENET1K_V1"))
        self.nclasses = nclasses
        self.model.requires_grad_(finetune)
        self.finetune = finetune
        self.__set_classification_head()
        self.__DEFAULT_MODEL_STATE = {
            k: v.shape for k,v in self.model.state_dict().items()
        }
    
    def __set_classification_head(self):
        self.model.fc = torch.nn.Linear(self.model.fc.in_features, self.nclasses)
    
    def forward(self, x):
        return self.model(x)

    def extract_features(self, x: torch.Tensor) -> torch.Tensor:
        classifier_names = ['fc', 'classifier', 'head', 'heads']
        for name, child in self.model.named_children():
            if name not in classifier_names:
                x = child(x)
        return x.flatten(start_dim=1)
    
    def extract_output_from(self, x: torch.Tensor, layer_name: str) -> torch.Tensor:
        layer_list = [(n,c) for n,c in self.model.named_children()]
        while len(layer_list) > 0:
            submodule_name, submodule = layer_list.pop(0)
            if submodule_name == layer_name: return submodule(x)
            if len([c for c in submodule.children()]) > 0:
                if not layer_name.startswith(submodule_name): x = submodule(x); continue
                [layer_list.insert(0+i, (f"{submodule_name}.{n}", c)) for i,(n,c) in enumerate(submodule.named_children())]; continue
            x = submodule(x)
        raise ModuleNotFoundError(f"{layer_name} not found in this model")
    
    def predict_features(self, f: torch.Tensor) -> torch.Tensor: return self.model.fc(f)
    
    def randinit(self): self.model.apply(self.__random_weight_init); return self
    
    def __random_weight_init(self, submodule: torch.nn.Module):
        if isinstance(submodule, torch.nn.Conv2d) or isinstance(submodule, torch.nn.Linear):
            torch.nn.init.xavier_uniform_(submodule.weight)
            if submodule.bias is not None: torch.nn.init.zeros_(submodule.bias)
    
    def default_state(self): return self.__DEFAULT_MODEL_STATE


if __name__ == "__main__":
    rn18 = ResNet18(100, False)
    print(rn18.model.fc.weight.shape)
