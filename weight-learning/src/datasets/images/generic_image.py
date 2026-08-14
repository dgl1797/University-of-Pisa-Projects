import torch
from PIL.Image import open as PIL_open
from torchvision.transforms import Compose, Resize, ToTensor

class GenericImagesDataset(torch.utils.data.Dataset):
    def __init__(self, rows: list[dict], filter_labels: list[int], transform = None):
        '''
        ### Arguments
            - rows: csv iterrows in the format containing names ["cid", "path"] as column selectors
            - filter_labels: list of labels to be filtered out
            - transform: transformations to be applied to the image dataset, 'default' will apply Compose([Resize((224, 224)), ToTensor()])
        '''
        assert len(rows) > 0, f"Argument 'rows' cannot be empty"
        assert all(["cid" in r and "path" in r for r in rows]), f"bad format, expected cids and path in rows got: {rows[0].keys()}"
        self.rows = [r for r in rows if r["cid"] not in filter_labels]
        self.cids_map = sorted(list(set([r["cid"] for r in self.rows])))
        self.transform = Compose([Resize((224, 224)), ToTensor()]) if transform == "default" else transform
    
    def __len__(self): return len(self.rows)
    
    def __getitem__(self, index):
        x,y = self.rows[index]["path"], self.cids_map.index(self.rows[index]["cid"])
        with PIL_open(x, "r") as img: x = img.convert("RGB") if self.transform is None else self.transform(img.convert("RGB"))
        return x,y