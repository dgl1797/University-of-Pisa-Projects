import sys, os, math

from torchvision.models import get_model, get_weight
from torch.nn import Module, Sequential, Linear, Conv2d, ReLU
from torch.nn.functional import mse_loss, cosine_similarity
from torch.nn.init import xavier_uniform_, zeros_
from torch import Tensor, tensor as torch_tensor, float as torch_float, no_grad, zeros as torch_zeros, cat as torch_cat,\
    empty as torch_empty
from numpy import ndarray
from typing import Any, Literal

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from src.utils.ConfigTypes import ClassifierInfo

class ClassifierModel(Module):
    def __init__(self, model_info: ClassifierInfo, n_classes: int, finetune: bool = False):
        super(ClassifierModel, self).__init__()
        self.model = get_model(model_info.name, weights=get_weight(model_info.weights))
        self.n_classes = n_classes
        self.finetune = finetune

        self.model.requires_grad_(finetune)

        self.__set_model_classifier(model_info.name, n_classes)
    
    def forward(self, x):
        return self.model(x)

    def extract_features(self, x: Tensor):
        classifier_names = ['fc', 'classifier', 'head', 'heads']
        for name, child in self.model.named_children():
            if name not in classifier_names:
                x = child(x)
        return x.flatten(start_dim=1)
    
    def extract_output_from(self, x: Tensor, layer_name: str) -> Tensor:
        layer_list = [(n,c) for n,c in self.model.named_children()]
        while len(layer_list) > 0:
            submodule_name, submodule = layer_list.pop(0)
            if submodule_name == layer_name: return submodule(x)
            if len([c for c in submodule.children()]) > 0:
                if not layer_name.startswith(submodule_name): x = submodule(x); continue
                [layer_list.insert(0+i, (f"{submodule_name}.{n}", c)) for i,(n,c) in enumerate(submodule.named_children())]; continue
            x = submodule(x)
        raise ModuleNotFoundError(f"{layer_name} not found in this model")
    
    def predict_features(self, f: Tensor) -> Tensor:
        if hasattr(self.model, 'fc'): return self.model.fc(f)
        elif hasattr(self.model, 'classifier'): return self.model.classifier(f)
        elif hasattr(self.model, 'head'): return self.model.head(f)
        elif hasattr(self.model, 'heads'): return self.model.heads(f)
        return None
    
    def randinit(self):
        self.model.apply(self.__random_weight_init)
        return self
    
    def get_classifier_weights(self) -> Tensor:
        '''
            returns the torch.cat of weights and biases along dimension 1, resulting in a shape (nclasses, nfeatures+1)
            torch.cat doesn't act inplace, this means returned tensor is a generated one that doesnt affect model's weights
        '''
        if hasattr(self.model, 'classifier'):
            if hasattr(self.model.classifier[-1], "in_features"): 
                # works for convnext, efficientnet, mobilenet, densenet, maxvit
                return torch_cat((self.model.classifier[-1].weight, self.model.classifier[-1].bias.unsqueeze(1)), dim=1).detach()
            elif hasattr(self.model.classifier[1], "in_channels"):
                pass # works for squeezenet - not implemented
            else: pass
        elif hasattr(self.model, "fc"):
            # works for resnet
            return torch_cat((self.model.fc.weight, self.model.fc.bias.unsqueeze(1)), dim=1).detach()
        elif hasattr(self.model, "head"):
            # works for swin
            return torch_cat((self.model.head.weight, self.model.fc.bias.unsqueeze(1)), dim=1).detach()
        elif hasattr(self.model, "heads"):
            # works for vit
            return torch_cat((self.model.heads[-1].weight, self.model.heads[-1].bias.unsqueeze(1)), dim=1).detach()
    
    def get_backbone_weights(self, keep_list: list[str] = []) -> Tensor:
        '''
            takes all the layers' weights of the model's backbone, in the same top-to-bottom order flattened over a single dimension

            **Args**:
                - keep_list: list[str] --> default is an empty list which means the method will return all the weights, 
                if a non-empty list is given, it will only return the keep_list layers' weights and biases, it is possible to pass
                entire submodules as "layer4" in resnet18 which will take the entire layer4 or even submodules like "layer4.1.bn2"
                which will return the weights and biases of only the second batch_norm in resnet18.layer4[1]
            
            **Returns**:
                a flattened tensor containing the ordered weights and biases (top-to-bottom order) of the layers in the current model.
                As it is obtained as a torch.cat of the accumulator + the module's weights and biases flattened it is a generated
                tensor that doesn't affect the model's weights
        '''
        layer_list = [(n,c) for n,c in self.model.named_children()]
        
        all_weights = torch_empty(0, dtype=torch_float)
        while(len(layer_list) > 0):
            submodule_name, submodule = layer_list.pop(0)

            if len([c for c in submodule.children()]) > 0: 
                [layer_list.insert(0+i, (f"{submodule_name}.{n}",c)) for i,(n,c) in enumerate(submodule.named_children())]; continue
            
            if not any([submodule_name.startswith(keep_layer) for keep_layer in keep_list]): continue
            if hasattr(submodule, "weight") and submodule.weight is not None:
                all_weights = torch_cat((all_weights, submodule.weight.view(-1)), dim=0)
            if hasattr(submodule, "bias") and submodule.bias is not None: 
                all_weights = torch_cat((all_weights, submodule.bias.view(-1)), dim=0)
        
        return all_weights.detach()

    def __set_model_classifier(self, model_name, n_classes):
        if hasattr(self.model, 'classifier'):
            if hasattr(self.model.classifier[-1], "in_features"): 
                # works for convnext, efficientnet, mobilenet, densenet, maxvit
                self.model.classifier[-1] = Linear(self.model.classifier[-1].in_features, n_classes).requires_grad_(self.finetune)
            elif hasattr(self.model.classifier[1], "in_channels"):
                # works for squeezenet
                self.model.classifier[1] = Conv2d(self.model.classifier[1].in_channels, n_classes, kernel_size=1, stride=1).requires_grad_(self.finetune)
            else: assert False, f"Model {model_name} not supported"
        elif hasattr(self.model, "fc"):
            # works for resnet
            self.model.fc = Linear(self.model.fc.in_features, n_classes).requires_grad_(self.finetune)
        elif hasattr(self.model, "head"):
            # works for swin
            self.model.head = Linear(self.model.head.in_features, n_classes).requires_grad_(self.finetune)
        elif hasattr(self.model, "heads"):
            # works for vit
            self.model.heads = Sequential(
                Linear(self.model.heads.head.in_features, n_classes)
            ).requires_grad_(self.finetune)
    
    def __random_weight_init(self, submodule: Module):
        if isinstance(submodule, Conv2d) or isinstance(submodule, Linear):
            xavier_uniform_(submodule.weight)
            if submodule.bias is not None: zeros_(submodule.bias)
    
class ZSLPredictor():
    def __init__(self, base: ClassifierModel, seen: list[int], unseen: list[int], attributes: ndarray, device: str, gzsl: bool):
        self.correction = 0
        self.__gzsl = gzsl
        if max(seen) == len(attributes[0]) or max(unseen) == len(attributes[0]): self.correction = 1
        self.seen_cids_map = sorted(seen)
        self.unsn_cids_map = sorted(unseen)
        self.seen_attributes = torch_tensor(attributes.tolist(), dtype=torch_float)[:, [i-self.correction for i in self.seen_cids_map]]
        self.candidate_attributes = torch_tensor(attributes.tolist(), dtype=torch_float)
        if not gzsl: self.candidate_attributes = self.candidate_attributes[:, [i-self.correction for i in self.unsn_cids_map]]
        self.base = base.to(device).requires_grad_(False).eval()
        self.device = device
    
    def to(self, device: str):
        self.base = self.base.to(device)
        self.device = device
        return self
    def state_dict(self):
        base_dictionary = {
            'seen_cids_map': self.seen_cids_map, 'unsn_cids_map': self.unsn_cids_map,
            'seen_attributes': self.seen_attributes, 'candidate_attributes': self.candidate_attributes,
            'correction': self.correction, 'model_state': self.base.model.state_dict()
        }
        return base_dictionary
    def requires_grad_(self, boolean: bool): self.base.model.requires_grad_(boolean); return self
    def load_state_dict(self, base_dictionary: dict[str, Any]):
        self.seen_cids_map = base_dictionary['seen_cids_map']; self.unsn_cids_map = base_dictionary['unsn_cids_map']
        self.seen_attributes = base_dictionary['seen_attributes']; self.candidate_attributes = base_dictionary['candidate_attributes']
        self.correction = base_dictionary['correction']
        return self.base.model.load_state_dict(base_dictionary['model_state'])
    def gzsl(self):
        if not self.__gzsl:
            attrs = torch_zeros((self.seen_attributes.shape[0], self.seen_attributes.shape[1]+self.candidate_attributes.shape[1]), dtype=torch_float, device=self.device)
            for i in range(attrs.shape[0]):
                for j in range(self.seen_attributes.shape[1]):
                    attrs[i, self.seen_cids_map[j]-self.correction] = self.seen_attributes[i,j]
                for j in range(self.candidate_attributes.shape[1]):
                    attrs[i, self.unsn_cids_map[j]-self.correction] = self.candidate_attributes[i,j]
            self.candidate_attributes = attrs
        return self
    def zsl(self):
        if self.__gzsl:
            attrs = self.candidate_attributes[:, [i-self.correction for i in self.unsn_cids_map]]
            self.candidate_attributes = attrs
        return self

    
    @no_grad()
    def predict(self, x: Tensor)->Tensor: pass

class ResidualBlock(Module):
    def __init__(self, Fn: Module):
        super(ResidualBlock, self).__init__()
        self.fn = Fn
    
    def forward(self, x: Tensor):
        x_hat: Tensor = self.fn(x)
        assert x_hat.shape == x.shape, f"Non-matching x shape with function's result shape"
        return x+x_hat

class AutoEncoder(Module):
    def __init__(self, nlayers: int, indim: int, latentdim: int, use_residual: bool = False):
        # assertions
        assert nlayers >= 2, f"Impossible to initialize an autoencoder with a nlayers < 2, at least 1 for encoding and 1 for decoding are required"
        
        # preparation
        super(AutoEncoder, self).__init__()
        self.inputs = indim
        self.latent = latentdim
        nencoders = nlayers // 2 + nlayers % 2
        ndecoders = nlayers // 2
        prevdim = indim

        encoder_arch: list[Module] = []
        for i in range(nencoders):
            newdim = max(prevdim // 2, latentdim) if nencoders-i > 1 else latentdim
            if prevdim == newdim and use_residual: encoder_arch.append(ResidualBlock(Sequential(Linear(prevdim, newdim), ReLU(inplace=True))))
            else: encoder_arch.append(Sequential(Linear(prevdim, newdim), ReLU(inplace=True)))
            prevdim = newdim
        
        decoder_arch: list[Module] = []
        for i in range(ndecoders):
            newdim = min(prevdim * 2, indim) if ndecoders-i > 1 else indim
            if ndecoders-i == 1: decoder_arch.append(Sequential(Linear(prevdim, newdim)))
            elif prevdim == newdim and use_residual: decoder_arch.append(ResidualBlock(Sequential(Linear(prevdim, newdim), ReLU(inplace=True))))
            else: decoder_arch.append(Sequential(Linear(prevdim, newdim), ReLU(inplace=True)))
            prevdim = newdim

        # initialization
        self.encoder = Sequential(*encoder_arch)
        self.decoder = Sequential(*decoder_arch)

    def forward(self, x: Tensor):
        return self.decoder(self.encoder(x))

class HyCUSBase(Module):
    def __init__(self, weights_dim: int, semantics_dim: int, latent_dim: int, align_coeff: float = 1.0):
        super(HyCUSBase, self).__init__()
        self.weights_ae = AutoEncoder(2, indim=weights_dim, latentdim=latent_dim)
        self.semants_ae = AutoEncoder(2, indim=semantics_dim, latentdim=latent_dim)
        self.align = align_coeff
    
    def forward(self, x: Tensor):
        weights, semantics = x
        z_weight = self.weights_ae.encoder(weights)
        z_semant = self.semants_ae.encoder(semantics)
        wtow = self.weights_ae.decoder(z_weight)
        wtos = self.semants_ae.decoder(z_weight)
        stos = self.semants_ae.decoder(z_semant)
        stow = self.weights_ae.decoder(z_semant)
        return wtow, stos, wtos, stow, z_weight, z_semant
    
    def loss(self, wtow: Tensor, stos: Tensor, wtos: Tensor, stow: Tensor, z_weight: Tensor, z_semant: Tensor, target_weights: Tensor, target_semantics: Tensor):
        wtow_loss = mse_loss(wtow, target_weights); stos_loss = mse_loss(stos, target_semantics)
        stow_loss = mse_loss(stow, target_weights); wtos_loss = mse_loss(wtos, target_semantics)
        align_loss = cosine_similarity(z_weight, z_semant).mean()
        return wtow_loss + wtos_loss + stos_loss + stow_loss + self.align * align_loss
    
    @no_grad()
    def make_weights(self, elements: Tensor, shared_size: int, shared_first: bool = True, use_weights: bool = True, aggregation: Literal["mean", "sum", "min", "max", "median", "none"] = "mean", device: str = "cpu") -> tuple[Tensor, Tensor]:
        '''
            **@Args**:
                - **elements**: either the weights tensor or the semantics tensor, depending on the use_weights value
                - **shared_size**: the size of the shared weights (generated weights will contain both shared and distinct weights)
                - **shared_first**: wether the shared weights are concatenated as first or not during training
                - **use_weights**: if True, elements are expected to be the weights of the target model, otherwise elements are expected to be the semantic descriptors
                - **aggregation**: how to aggregate shared weights, default is mean
                - **device**: where the computation must happen, default is cpu
            
            **@Returns**:
                - shared weights as first return element, distinct weights as second return element
        '''
        allowed_aggregations = ["mean", "sum", "min", "max", "median", "none"]
        assert aggregation in allowed_aggregations, f"{aggregation} method not supported, only {[ag for ag in allowed_aggregations]} are"
        prev_device = next(self.parameters()).device
        self = self.to(device).eval()
        elements = elements.to(device)
        gw: Tensor = self.weights_ae.decoder(self.weights_ae.encoder(elements) if use_weights else self.semants_ae.encoder(elements))
        distinct_size = gw.shape[1]-shared_size
        ws = gw[:, :shared_size] if shared_first else gw[:, distinct_size:]
        wd = gw[:, shared_size:] if shared_first else gw[:, :distinct_size]
        if aggregation != "none": ws: Tensor = getattr(ws, aggregation)(dim=0) if aggregation not in ["min", "max", "median"] else getattr(ws, aggregation)(dim=0).values

        self.to(prev_device); return ws.to("cpu"), wd.to("cpu")

    @no_grad()
    def make_semants(self, elements: Tensor, use_weights: bool = False, device: str = "cpu"):
        self.to(device).eval(); elements = elements.to(device)
        gsm: Tensor = self.semants_ae.decoder(self.weights_ae.encoder(elements) if use_weights else self.semants_ae.encoder(elements))
        
        self.to("cpu"); return gsm.to("cpu")