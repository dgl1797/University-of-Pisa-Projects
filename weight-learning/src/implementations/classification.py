import os, sys, torch
from tqdm import tqdm
from numpy import ndarray, save as npysave, load as npyload
from pathlib import Path

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.models.classifiers.resnet18 import ResNet18

class ClassificationTask():
    def __init__(self, taskname: str, nclasses: int, device: str = "cpu"):
        self.device = device
        self.nclasses = nclasses
        self.confusion_matrix = torch.zeros((nclasses, nclasses), requires_grad=False)
        if taskname:
            self.store_location = Path("out", "tasks", "classification")
            self.store_location.mkdir(777, parents=True, exist_ok=True)
            self.store_location = self.store_location.joinpath(f"{taskname}.npy")
    
    def __zero(self): 
        self.confusion_matrix = torch.zeros((self.nclasses, self.nclasses), dtype=torch.float, requires_grad=False)
        self.accuracy = None; self.precision = None; self.recall = None; self.f1score = None

    @torch.no_grad()
    def __call__(self, classifier: ResNet18, dataloader: torch.utils.data.DataLoader, desc: str = "Evaluating"):
        self.__zero(); classifier.to(self.device).eval(); cm = self.confusion_matrix.to(self.device)
        for pair in tqdm(dataloader, desc=desc):
            img, lbl = [p.to(self.device) for p in pair]
            y_pred: torch.Tensor = classifier.forward(img)
            if y_pred.ndim > 1: y_pred = torch.argmax(y_pred, dim=1)
            cm += torch.bincount(
                lbl*self.nclasses+y_pred, minlength=self.nclasses**2
            ).reshape(self.nclasses, self.nclasses)
        
        classifier.to("cpu")
        self.confusion_matrix = cm.to("cpu")
        return self.compute_dictionary()
    
    def compute_dictionary(self):
        cm = self.confusion_matrix.to(self.device)
        total_guesses = cm.diag().sum()
        total_elements = cm.sum()
        true_positive = cm.diag()
        false_positive = cm.sum(dim=0)-true_positive
        false_negative = cm.sum(dim=1)-true_positive
        self.accuracy = torch.where(total_elements == 0, torch.tensor(1.0, device=self.device), total_guesses/total_elements).item()
        self.precision = torch.where(true_positive+false_positive == 0, torch.tensor(1.0, device=self.device), true_positive/(true_positive+false_positive)).mean().item()
        self.recall = torch.where(true_positive+false_negative == 0, torch.tensor(1.0, device=self.device), true_positive/(true_positive+false_negative)).mean().item()
        self.f1score = 2*self.precision*self.recall/(self.precision+self.recall)
        return {
            "accuracy": self.accuracy, "precision": self.precision, "recall": self.recall, "f1": self.f1score
        }

    def get_state(self): return self.confusion_matrix.numpy()
    def set_state(self, confusion_matrix: ndarray): self.confusion_matrix = torch.tensor(confusion_matrix)
    def todict(self): return {
        "accuracy": self.accuracy, "precision": self.precision, "recall": self.recall, "f1": self.f1score
    }
    def save_state(self): self.store_location.unlink(missing_ok=True); npysave(self.store_location, self.confusion_matrix.numpy())
    def load_state(self): self.confusion_matrix = torch.tensor(npyload(self.store_location))

if __name__ == "__main__":
    from src.datasets.images.generic_image import GenericImagesDataset
    from src.datasets.images.helpers import load_rows_from_csv, load_txt
    from src.models.classifiers.resnet18 import ResNet18
    
    csvloc = Path("data", "AWA2_Data", "image_splits", "base", "test.csv")
    fltloc = Path("data", "AWA2_Data", "class_splits", "tunseen.txt")
    imgds = GenericImagesDataset(load_rows_from_csv(csvloc), load_txt(fltloc), transform="default")

    classificationtask = ClassificationTask("sane_injected_test", len(imgds.cids_map), "cuda")
    injected_checkpoint = torch.load(Path("out", "sane", "injections", "tester_injection.pt"))
    original_checkpoint = torch.load(Path("checkpoints", "resnet18_awa2.pt"))
    classifier_network = ResNet18(len(imgds.cids_map), False)
    classifier_network.load_state_dict(injected_checkpoint)
    metrics_dictionary = classificationtask(
        classifier_network,
        torch.utils.data.DataLoader(imgds, 32, False, num_workers=4, persistent_workers=True)
    )
    print(metrics_dictionary)