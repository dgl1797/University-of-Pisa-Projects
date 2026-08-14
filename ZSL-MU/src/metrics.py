import os,sys
from torch import no_grad, Tensor, float as torch_float, zeros as torch_zeros, argmax as torch_argmax, bincount,\
    tensor as torch_tensor, where, empty as torch_empty, cat as torch_cat,\
    ones_like as torch_oneslike, zeros_like as torch_zeroslike
from torch.utils.data import DataLoader
from torch.nn import CrossEntropyLoss
from torch.nn.functional import cosine_similarity, mse_loss
from numpy import ndarray
from math import isnan
from typing import Literal
from sklearn.model_selection import StratifiedShuffleSplit, cross_val_score
from sklearn.linear_model import LogisticRegression
from numpy import array as npyarray
from numpy.random import choice as npychoice

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from src.utils.Interfaces import EvaluationBase
from src.models import ClassifierModel

class ClassificationEval(EvaluationBase):
    def __init__(self, nclasses: int, device: str, fromfile: str = None):
        self.nclasses = nclasses
        self.device = device
        if fromfile is not None and os.path.exists(f"{fromfile}.npy"): self.load(fromfile); self.loaded = True; return None
        
        self.cm: Tensor = torch_zeros((nclasses, nclasses), dtype=torch_float, requires_grad=False)
    
    def __call__(self, y_pred, y_true):
        preds = y_pred.to(self.device); lbls = y_true.to(self.device); cm = self.cm.to(self.device)
        if isinstance(preds.tolist()[0], list): preds = torch_argmax(preds, dim=1)
        # bincount of the 1D flattening of CM, where lbls works as row counter (y_true on rows) and preds as column counter
        cm += bincount(lbls*self.nclasses+preds, minlength=self.nclasses**2).reshape(self.nclasses, self.nclasses)
        self.cm = cm.to("cpu")
    
    def __str__(self):
        '''
            Uses todict() dictionary of tuples where first element is the metric value, the second element is a boolean
            indicating if the metric must be converted into a percentage and generates a string of the retrieved dictionary
        '''
        return(repr({k: f"{f'{v[0]*100:.2f}%' if v[1] else f'{v[0]:.4f}'}" for k,v in self.todict().items()}))

    def accuracy(self):
        cm = self.cm.to(self.device) # creates a copy of self.cm into GPU Memory storing the copy's reference in cm variable
        total_guesses = cm.diag().sum()
        total_elments = cm.sum(dim=1).sum()
        return where(total_elments == 0, torch_tensor(1.0, device=self.device), total_guesses/total_elments).item()
    
    def precision(self):
        cm = self.cm.to(self.device)
        tp = cm.diag()
        denom = cm.sum(dim=0) # fp = cm.sum(dim=0)-tp, denom = tp+fp => cm.sum(dim=0)
        return where(denom == 0, torch_tensor(1.0, device=self.device), tp/denom).mean().item()
    
    def recall(self):
        cm = self.cm.to(self.device)
        tp = cm.diag()
        denom = cm.sum(dim=1) # fn = cm.sum(dim=1)-tp, denom = tp+fn => cm.sum(dim=1)
        return where(denom == 0, torch_tensor(1.0, device=self.device), tp/denom).mean().item()
    
    def f1score(self):
        precision = self.precision(); recall = self.recall()
        return 2*precision*recall/(precision+recall) # harmonic mean of precision and recall
    
    def get_state(self) -> ndarray:
        return self.cm.numpy()
    def set_state(self, state: ndarray):
        self.cm = torch_tensor(state.tolist(), dtype=torch_float, requires_grad=False)
        return None
    
    def zero(self) -> None:
        self.cm = torch_zeros((self.nclasses, self.nclasses), dtype=torch_float, requires_grad=False)
        return None
    def todict(self) -> dict[str, float]:
        return {
            'Accuracy': (self.accuracy(), True), 'Precision': (self.precision(), True), 
            'Recall': (self.recall(), True), 'F1': (self.f1score(), True)
        }

class UnlearnEval(ClassificationEval):
    def __init__(self, nclasses: int, device: str, forget: list[int], origin_raccuracy: float = None, fromfile: str = None):
        self.device = device
        self.nclasses = nclasses
        self.forget = forget
        self.retain = [i for i in range(nclasses) if i not in forget]
        self.origin_raccuracy = origin_raccuracy
        if fromfile is not None and os.path.exists(f"{fromfile}.mat"): self.load(fromfile); self.loaded = True; return None

        self.cm = torch_zeros((nclasses, nclasses), dtype=torch_float, requires_grad=False)
        self.fdeltas = torch_empty(0, dtype=torch_float, requires_grad=False) # features delta
        self.ddeltas = torch_empty(0, dtype=torch_float, requires_grad=False) # distribs delta
    
    def accuracy(self, retain: bool = True):
        cm = self.cm.to(self.device)
        indexes = self.retain if retain else self.forget
        tpi = cm.diag()[indexes].sum()
        tot = cm.sum(dim=1)[indexes].sum()
        return (tpi/tot).item() if tot.item() != 0.0 else 1.0
    
    def AUS(self):
        assert self.origin_raccuracy is not None, f"expert model's retain accuracy unset but necessary to evaluate AUS"
        assert self.origin_raccuracy >= 0 and self.origin_raccuracy <= 1, f"accuracy must be in [0,1] but got: {self.origin_raccuracy:.4f}"
        racc = self.accuracy(retain=True)
        facc = self.accuracy(retain=False)
        return (1 + (racc-self.origin_raccuracy))/(1+facc)
    
    def distributions_deltas(self, expert_y: Tensor, unlearn_y: Tensor):
        expert_y = expert_y.to(device=self.device).softmax(dim=1); unlearn_y = unlearn_y.to(device=self.device).softmax(dim=1)
        M = ((expert_y+unlearn_y)*0.5)
        # conventionally KL(0 || 0) has P=0 to happen hence all is converted to 0
        expert_kl = where(expert_y != 0, (expert_y * (expert_y/M).log()), torch_tensor(0.0, device=self.device)).sum(dim=1)
        unlearn_kl = where(unlearn_y != 0, (unlearn_y * (unlearn_y/M).log()), torch_tensor(0.0, device=self.device)).sum(dim=1)
        batch_jsd = (0.5*expert_kl + 0.5*unlearn_kl).mean(dim=0)
        self.ddeltas = torch_cat((self.ddeltas, batch_jsd.unsqueeze(0).to("cpu")), dim=0)
        return None
    def JSD(self):
        return self.ddeltas.mean().item() if not isnan(self.ddeltas.mean().item()) else 0.0
    
    def todict(self, train=False) -> dict[str, float]:
        return {
            'Retain Accuracy': (self.accuracy(retain=True), True), 
            'Forget Accuracy': (self.accuracy(retain=False), True), 
            'AUS': (self.AUS(), False), 
            'JSD': (self.JSD(), False)
        } if not train else {
            'racc': (self.accuracy(retain=True), False),
            'facc': (self.accuracy(retain=False), False)
        }

    def get_state(self):
        return {
            'cm': self.cm.numpy(),
            'fdeltas': self.fdeltas.numpy(),
            'ddeltas': self.ddeltas.numpy(),
        }
    def set_state(self, state):
        self.cm = torch_tensor(state['cm'].tolist(), dtype=torch_float)
        self.fdeltas = torch_tensor(state['fdeltas'].tolist(), dtype=torch_float)
        self.ddeltas = torch_tensor(state['ddeltas'].tolist(), dtype=torch_float)

    def save(self, location):
        from scipy.io import savemat
        if os.path.exists(f"{location}.mat"): os.remove(f"{location}.mat")
        return savemat(f"{location}.mat", self.get_state())
    def load(self, location):
        from scipy.io import loadmat
        matcontent = loadmat(f"{location}.mat")
        self.set_state(matcontent)
        return self.get_state()
    
class ZSLearnEval(ClassificationEval):
    def __init__(self, nclasses: int, seen: list[int], unseen: list[int], device: str):
        super().__init__(nclasses, device)
        self.correction = 0
        if max(unseen) == nclasses or max(seen) == nclasses: self.correction = 1
        self.unseen = sorted([i-self.correction for i in unseen])
        self.seen = sorted([i-self.correction for i in seen])
    
    def store_accuracy(self): self.stored_accuracy = self.accuracy()

    def zsl_accuracy(self, unseen: bool):
        cm = self.cm.to(self.device); index_set = self.unseen if unseen else self.seen
        tp = cm.diag()[index_set].sum()
        dn = cm.sum(dim=1)[index_set].sum()
        return tp/dn if dn != 0.0 else 1.0
    
    def harmonic_accuracy(self):
        u = self.zsl_accuracy(unseen=True); s = self.zsl_accuracy(unseen=False)
        return 2*u*s/(u+s)
    
    def todict(self):
        return {
            'Accuracy': (self.stored_accuracy, True),
            'Generic Accuracy': (self.accuracy(), True),
            'Unseen Accuracy': (self.zsl_accuracy(unseen=True), True),
            'Seen Accuracy': (self.zsl_accuracy(unseen=False), True),
            'Harmonic Accuracy': (self.harmonic_accuracy(), True)
        }

class MIAEval(EvaluationBase):
    def __init__(self, target_model: ClassifierModel, members_loader: DataLoader, nonmembers_loader: DataLoader, device: str, fileloc: str = None):
        self.device = device
        if fileloc is not None and os.path.exists(f"{fileloc}_mia.mat"): self.load(fileloc); self.loaded = True; return None
        
        self.features = torch_empty(0, dtype=torch_float, requires_grad=False)
        self.logits = torch_empty(0, dtype=torch_float, requires_grad=False)
        self.losses = torch_empty(0, dtype=torch_float, requires_grad=False)
        self.members = torch_empty(0, dtype=torch_float, requires_grad=False)
        
        print("Loading MIA components")
        with no_grad():
            target_model.eval().to(device)
            loss_criterion = CrossEntropyLoss(reduction='none')
            for x,y in members_loader:
                features = target_model.extract_features(x.to(self.device))
                logits = target_model.predict_features(features)
                losses = loss_criterion(logits, y.to(self.device)).unsqueeze(1)
                self.features = torch_cat((self.features, features.to("cpu")), dim=0)
                self.logits = torch_cat((self.logits, logits.to("cpu")), dim=0)
                self.losses = torch_cat((self.losses, losses.to("cpu")), dim=0)
                self.members = torch_cat((self.members, torch_oneslike(y, device="cpu")), dim=0)
            for x,y in nonmembers_loader:
                features = target_model.extract_features(x.to(self.device))
                logits = target_model.predict_features(features)
                losses = loss_criterion(logits, y.to(self.device)).unsqueeze(1)
                self.features = torch_cat((self.features, features.to("cpu")), dim=0)
                self.logits = torch_cat((self.logits, logits.to("cpu")), dim=0)
                self.losses = torch_cat((self.losses, losses.to("cpu")), dim=0)
                self.members = torch_cat((self.members, torch_zeroslike(y, device="cpu")), dim=0)
            target_model.to("cpu")

    def __str__(self):
        '''
            Uses todict() dictionary of tuples where first element is the metric value, the second element is a boolean
            indicating if the metric must be converted into a percentage and generates a string of the retrieved dictionary
        '''
        return(repr({k: f"{f'{v[0]*100:.2f}%' if v[1] else f'{v[0]:.4f}'}" for k,v in self.todict().items()}))
    
    def __call__(self, features: Tensor, logits: Tensor, losses: Tensor, membership: Tensor):
        ''' adds features, logits, losses and membership to current state '''
        self.features = torch_cat((self.features, features.to("cpu")), dim=0)
        self.logits = torch_cat((self.logits, logits.to("cpu")), dim=0)
        self.losses = torch_cat((self.losses, losses.to("cpu")), dim=0)
        self.members = torch_cat((self.members, membership.to("cpu")), dim=0)


    def MIAScore(self, mode: Literal["features", "logits", "losses"], nfolds: int):
        assert mode in ["features", "logits", "losses"], f"Unknown MIA version, only supported on 'features', 'logits' or 'losses'"

        memberdata = torch_tensor([idx for idx,lbl in enumerate(self.members) if lbl])
        nonmembers = torch_tensor([idx for idx,lbl in enumerate(self.members) if not lbl])

        attack_model = LogisticRegression(max_iter=2000)
        dataset_idxs = torch_cat((memberdata, nonmembers), dim=0)
        cross_validator = StratifiedShuffleSplit(nfolds)
        elements = getattr(self, mode)[dataset_idxs].numpy()
        labels = self.members[dataset_idxs].numpy()
        mia_scores = cross_val_score(attack_model, elements, labels, cv=cross_validator, scoring="accuracy")

        return mia_scores.mean()
    
    def todict(self, nfolds: int = 10, attacks: list[Literal["features", "logits", "losses"]] = ["features", "losses"]) -> dict[str, float]:
        translation = {"features": "fMIA", "logits": "pMIA", "losses": "lMIA"}
        generated_metrics: dict[str, tuple[float, bool]] = {}
        for i,attack in enumerate(attacks): generated_metrics.update({translation[attack]: (self.MIAScore(attack, nfolds), True)})
        return generated_metrics

    def get_state(self):
        return {
            'features': self.features.numpy(),
            'logits': self.logits.numpy(),
            'losses': self.losses.numpy(),
            'members': self.members.numpy()
        }
    def set_state(self, state):
        self.features = torch_tensor(state['features'].tolist(), dtype=torch_float)
        self.logits = torch_tensor(state['logits'].tolist(), dtype=torch_float)
        self.losses = torch_tensor(state['losses'].tolist(), dtype=torch_float)
        self.members = torch_tensor(state['members'].squeeze(0).tolist(), dtype=torch_float)

    def save(self, location):
        from scipy.io import savemat
        if os.path.exists(f"{location}_mia.mat"): os.remove(f"{location}_mia.mat")
        return savemat(f"{location}_mia.mat", self.get_state())
    def load(self, location):
        from scipy.io import loadmat
        matcontent = loadmat(f"{location}_mia.mat")
        self.set_state(matcontent)
        return self.get_state()

class HyCUSEval(EvaluationBase):
    def __init__(self, generated_ws: Tensor, generated_wd: Tensor, origin_ws: Tensor, origin_wd: Tensor, golden_ws: Tensor, golden_wd: Tensor, generated_sm: Tensor, original_sm: Tensor, forgetset: list[int] = [], device: str = "cpu"):
        self.gwd = generated_wd.to("cpu"); self.gws = generated_ws.to("cpu")
        self.owd = origin_wd.to("cpu"); self.ows = origin_ws.to("cpu")
        self.gowd = golden_wd.to("cpu"); self.gows = golden_ws.to("cpu")
        self.gsm = generated_sm.to("cpu"); self.osm = original_sm.to("cpu")
        self.forgetset = forgetset
        self.retainset = [i for i in range(self.gwd.shape[0]) if i not in forgetset]
        self.device = device
    
    def __str__(self): 
        '''
            Uses todict() dictionary of tuples where first element is the metric value, the second element is a boolean
            indicating if the metric must be converted into a percentage and generates a string of the retrieved dictionary
        '''
        return(repr({k: f"{f'{v[0]*100:.2f}%' if v[1] else f'{v[0]:.4f}'}" for k,v in self.todict().items() if not isinstance(v[0], ndarray)}))

    def todict(self):
        result_dict: dict[str, tuple[float, bool]] = {}

        # shared histograms
        result_dict["shared_unlearn"] = (self.gws.mean(dim=0).flatten().numpy(), False)
        result_dict["shared_origin"] = (self.ows.flatten().numpy(), False)
        result_dict["shared_golden"] = (self.gows.flatten().numpy(), False)

        # distinct retain histograms
        result_dict["distinct_retain_unlearn"] = (self.gwd[self.retainset].flatten().numpy(), False)
        result_dict["distinct_retain_origin"] = (self.owd[self.retainset].flatten().numpy(), False)
        result_dict["distinct_retain_golden"] = (self.gowd[self.retainset].flatten().numpy(), False)
        # distinct forget histograms
        result_dict["distinct_forget_unlearn"] = (self.gwd[self.forgetset].flatten().numpy(), False)
        result_dict["distinct_forget_origin"] = (self.owd[self.forgetset].flatten().numpy(), False)
        result_dict["distinct_forget_golden"] = (self.gowd[self.forgetset].flatten().numpy(), False)

        # semantics histograms
        result_dict["semantics_retain_unlearn"] = (self.gsm[self.retainset].flatten().numpy(), False)
        result_dict["semantics_retain_original"] = (self.osm[self.retainset].flatten().numpy(), False)
        result_dict["semantics_forget_unlearn"] = (self.gsm[self.forgetset].flatten().numpy(), False)
        result_dict["semantics_forget_original"] = (self.osm[self.forgetset].flatten().numpy(), False)

        return result_dict
    
    def get_state(self):
        return {
            "generated_distinct": self.gwd.numpy(), "generated_shared": self.gws.numpy(),
            "origin_distinct": self.owd.numpy(), "origin_shared": self.ows.numpy(),
            "golden_distinct": self.gowd.numpy(), "golden_shared": self.gows.numpy(),
            "generated_semants": self.gsm.numpy(), "original_semants": self.osm.numpy(),
            "forgetset": npyarray(self.forgetset)
        }
    def set_state(self, state):
        self.gwd = torch_tensor(state["generated_distinct"].tolist()); self.gws = torch_tensor(state["generated_shared"].tolist())
        self.owd = torch_tensor(state["origin_distinct"].tolist()); self.ows = torch_tensor(state["origin_shared"].tolist())
        self.gowd = torch_tensor(state["golden_distinct"].tolist()); self.gows = torch_tensor(state["golden_shared"].tolist())
        self.gsm = torch_tensor(state["generated_semants"].tolist()); self.osm = torch_tensor(state["original_semants"].tolist())
        self.forgetset = state["forgetset"].flatten().tolist()
        self.retainset = [i for i in range(self.gwd.shape[0]) if i not in self.forgetset]

    def save(self, location):
        from scipy.io import savemat
        if os.path.exists(f"{location}_hycus.mat"): os.remove(f"{location}_hycus.mat")
        return savemat(f"{location}_hycus.mat", self.get_state())
    
    def load(self, location):
        from scipy.io import loadmat
        matcontent = loadmat(f"{location}_hycus.mat")
        self.set_state(matcontent)
        return self.get_state()