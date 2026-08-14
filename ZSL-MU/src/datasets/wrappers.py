from torch.utils.data import Dataset
from PIL.Image import open as PIL_open
from typing import Literal
from numpy import ndarray
from numpy.random import choice as npchoice
from torch import tensor as torch_tensor, bool as torch_bool, float as torch_float, int as torch_int

import os

class ZSLMUBase(Dataset):
    def __init__(self, rows: list[dict], transform = None, iter_set: str = "full"):
        assert len(rows) > 0, f"Argument 'rows' cannot be empty"
        assert all(["cid" in r and "path" in r for r in rows]), f"bad format, expected cids and path in rows got: {rows[0].keys()}"
        self.rows = rows
        self.datasets = {}
        self.datasets["full"] = range(len(rows))
        self.__ITER_SET = iter_set
        self.transform = transform
        self.cids_map = sorted(list(set([r["cid"] for r in rows])))
    
    def __len__(self): return len(self.datasets[self.__ITER_SET])
    def __iter__(self): return iter([r for i,r in enumerate(self.rows) if i in self.datasets[self.__ITER_SET]])
    
    def __contains__(self, item: dict[str, str]):
        assert "cid" in item and "path" in item, f"item badly formatted, expected to have cid,path keys but got: {item.keys()}"
        selected_dataset = self.datasets[self.__ITER_SET]
        return any([item["cid"] == r["cid"] and item["path"] == r["path"] for i,r in enumerate(self.rows) if i in selected_dataset])
    
    def __getitem__(self, index):
        index = self.datasets[self.__ITER_SET][index]
        x,y = self.rows[index]["path"], self.cids_map.index(self.rows[index]["cid"])
        file_path = x.replace('\\', '/')  # Convert Windows backslashes to Unix forward slashes
        with PIL_open(file_path, "r") as img: 
            x = img.convert("RGB") if self.transform is None else self.transform(img.convert("RGB"))      
        return x,y
    
    def get_iter_set(self): return self.__ITER_SET
    def nclasses(self)->int: return len(self.cids_map)
    def translate_labels(self, lbl):
        '''accepts both a list of labels or single label as numeric ids, returns the dataset id for those labels'''
        if isinstance(lbl, list) and all([isinstance(lbl[i], int) for i in range(len(lbl))]): return [self.cids_map.index(y) for y in lbl]
        if isinstance(lbl, int): return self.cids_map.index(lbl)
        raise TypeError(f"{type(lbl)} not supported, use either list[int] or int types as argument")
    def real_labels(self, lbl):
        '''reverts dataset translation returning the real ids of the passed label[s]'''
        if isinstance(lbl, list) and all([isinstance(lbl[i], int) for i in range(len(lbl))]): return [self.cids_map[y] for y in lbl]
        if isinstance(lbl, int): return self.cids_map[lbl]
        raise KeyError(f"{type(lbl)} not supported, use either list[int] or int types as argument")

class ZSLDataset(ZSLMUBase):
    def __init__(self, rows: list[dict], unseen: list[int] = [], transform = None, iter_set: Literal["full", "seen", "unseen"] = "seen"):
        super().__init__(rows, transform, iter_set)
        self.unseen = sorted(unseen)
        self.datasets["seen"] = [i for i,r in enumerate(rows) if r["cid"] not in unseen]
        self.datasets["unseen"] = [i for i,r in enumerate(rows) if r["cid"] in unseen]
        if iter_set == "seen": self.cids_map = sorted(list(set([r["cid"] for r in rows if r["cid"] not in unseen])))
        if iter_set == "unseen": self.cids_map = sorted(list(set([r["cid"] for r in rows if r["cid"] in unseen])))
    
    def on_unseen(self): return ZSLDataset(self.rows, self.unseen, self.transform, "unseen")
    def on_seen(self): return ZSLDataset(self.rows, self.unseen, self.transform, "seen")
    def on_full(self): return ZSLDataset(self.rows, self.unseen, self.transform, "full")
    def sample_like(self, other: object):
        assert isinstance(other, ZSLDataset), f"{type(other)} not supported for this operation"
        selected_indexes: list[int] = []
        for cid in self.cids_map: selected_indexes.extend(npchoice([i for i,r in enumerate(self.rows) if r["cid"] == cid], len([r for r in other.rows if r["cid"] == cid]), replace=False).tolist())
        return ZSLDataset([r for i,r in enumerate(self.rows) if i in selected_indexes], self.unseen, self.transform, self.get_iter_set())


class MULDataset(ZSLMUBase):
    def __init__(self, rows: list[dict], forget: list[int] = [], transform = None, iter_set: Literal["full", "retain", "forget"] = "retain"):
        super().__init__(rows, transform, iter_set)
        self.forget = sorted(forget)
        self.datasets["retain"] = [i for i,r in enumerate(rows) if r["cid"] not in forget]
        self.datasets["forget"] = [i for i,r in enumerate(rows) if r["cid"] in forget]
    
    def on_forget(self): return MULDataset(self.rows, self.forget, self.transform, "forget")
    def on_retain(self): return MULDataset(self.rows, self.forget, self.transform, "retain")
    def on_full(self): return MULDataset(self.rows, self.forget, self.transform, "full")
    def sample_like(self, other: object):
        assert isinstance(other, MULDataset), f"{type(other)} not supported for this operation"
        selected_indexes: list[int] = []
        for cid in self.cids_map: selected_indexes.extend(npchoice([i for i,r in enumerate(self.rows) if r["cid"] == cid], len([r for r in other.rows if r["cid"] == cid]), replace=False).tolist())
        return MULDataset([r for i,r in enumerate(self.rows) if i in selected_indexes], self.forget, self.transform, self.get_iter_set())

    def __mul__(self, other: float):
        '''
            dataset * x --> takes (x*100)% rightmost elements of the currently selected dataset to be kept, filtering per-class
            so all classes are kept, **this method will permanently delete data from the dataset**
        '''
        other = 1-other # let's say i want the 80% of the leftmost elements, i have to remove 20% of the rightmost
        assert isinstance(other, float) and other<=1.0 and other>=0, f"{other} not a float in [0, 1]"
        deleted_indexes = []
        residual = 0
        selected_dataset = self.datasets[self.get_iter_set()]
        for cid in self.cids_map:
            # taking only selected dataset's indexes to be removed
            selected_indexes = [i for i,r in enumerate(self.rows) if r["cid"] == cid and i in selected_dataset]
            final_index = int(round(other*len(selected_indexes) + residual))
            residual = final_index - other*len(selected_indexes)
            selected_indexes = selected_indexes[:final_index]
            deleted_indexes.extend(selected_indexes)
        return MULDataset([r for i,r in enumerate(self.rows) if i not in deleted_indexes], self.forget, self.transform, self.get_iter_set())
    def __rmul__(self, other: float):
        '''
            x * dataset --> takes the (x*100)% leftmost elements of the currently selected dataset to be kept, filtering per-class
            so that all class are kept, **this method will permanently delete data from the dataset**
        '''
        # eliminating 1-0.8 of the rightmost, corresponds in eliminating starting from the 20th% element to the end
        assert isinstance(other, float) and other<=1.0 and other>=0, f"{other} not a float in [0, 1]"
        deleted_indexes = []
        residual = 0
        selected_dataset = self.datasets[self.get_iter_set()]
        for cid in self.cids_map:
            selected_indexes = [i for i,r in enumerate(self.rows) if r["cid"] == cid and i in selected_dataset]
            starting_index = int(round(other*len(selected_indexes) + residual))
            residual = starting_index - other*len(selected_indexes)
            selected_indexes = selected_indexes[starting_index:]
            deleted_indexes.extend(selected_indexes)
        return MULDataset([r for i,r in enumerate(self.rows) if i not in deleted_indexes], self.forget, self.transform, self.get_iter_set())
    
class ICUSDataset(Dataset):
    def __init__(self, isforget: ndarray, descriptions: ndarray, wshared: ndarray, wdistinct: ndarray, cids: list[int]):
        super().__init__()
        self.cids = cids
        self.data = list(zip(isforget, descriptions, wshared, wdistinct, cids))
        self.nclasses = len(cids)
        self.weights_dimension = wshared.shape[1] + wdistinct.shape[1]
        self.semants_dimension = descriptions.shape[1]
    
    def __len__(self):
        return len(self.data)

    def __getitem__(self, index):
        isforget, description, wshared, wdistinct, label = self.data[index]
        return torch_tensor(isforget, dtype=torch_bool), torch_tensor(description, dtype=torch_float),\
            torch_tensor(wshared, dtype=torch_float), torch_tensor(wdistinct, dtype=torch_float),\
            torch_tensor(label, dtype=torch_int)
        