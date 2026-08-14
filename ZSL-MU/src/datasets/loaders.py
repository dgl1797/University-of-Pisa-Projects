import os
from pandas import read_csv
from numpy import array, ndarray, load as npyload
from .wrappers import ZSLDataset, MULDataset, ICUSDataset

def load_classification_sets(split_loc: str, unseen_file: str = None, transform = None):
    '''
        takes the split location path, the unseen.txt file and returns the train, test and validation splits
        loads train test and valid splits from split_loc and generates ZSLDatasets
    '''
    unseen = load_txt(unseen_file) if unseen_file is not None else []
    assert all([sn in ['train', 'test', 'valid'] for sn in [s.split(".")[0] for s in os.listdir(split_loc)]]), f"expected splits: ['train', 'test', 'valid'] not found in {split_loc}"
    
    train_rows = [r for _,r in read_csv(os.path.join(split_loc, "train.csv"), sep=";", names=["cid", "path"], dtype={'cid': int, 'path': str}).iterrows()]
    valid_rows = [r for _,r in read_csv(os.path.join(split_loc, "valid.csv"), sep=";", names=["cid", "path"], dtype={'cid': int, 'path': str}).iterrows()]
    test_rows  = [r for _,r in read_csv(os.path.join(split_loc, "test.csv"), sep=";", names=["cid", "path"], dtype={'cid': int, 'path': str}).iterrows()]

    return ZSLDataset(train_rows, unseen, transform), ZSLDataset(test_rows, unseen, transform), ZSLDataset(valid_rows, unseen, transform)

def load_classification_fsets(split_loc: str, unseen_file: str = None, forget_file: str = None, transform = None):
    '''
        takes the split location path, the unseen.txt file, the forget.txt file and returns train, test and validation splits
        load train test and valid splits from split_loc and generates MUDatasets
    '''
    trainZS, testZS, validZS = load_classification_sets(split_loc, unseen_file, transform)

    forget = load_txt(forget_file)

    # ZSLMUBase iters on selected rows only
    return MULDataset([r for r in trainZS], forget, transform, "retain"),\
           MULDataset([r for r in testZS], forget, transform, "retain"),\
           MULDataset([r for r in validZS], forget, transform, "retain")

def load_icus_dataset(forget_loc: str, unseen_loc: str, classes_loc: str, semants_loc: str, origin):
    unseencids = sorted(load_txt(unseen_loc))
    forgetcids = sorted(load_txt(forget_loc))
    allcids = sorted([r["cid"] for _,r in read_csv(classes_loc, sep=";", names=["cid", "name"]).iterrows()])
    seencids = sorted([cid for cid in allcids if cid not in unseencids])
    seenindexes = [allcids.index(scid) for scid in seencids]

    semantics: ndarray = npyload(semants_loc).T
    shared = origin.get_backbone_weights(keep_list=["layer4.1.bn2"])
    distinct = origin.get_classifier_weights()

    # dataset construction
    isforget = array([seencids[i] in forgetcids for i in range(len(seencids))])
    descriptions = semantics[seenindexes, :]
    shared = shared.numpy()[None, :].repeat(len(seencids), axis=0)
    distinct = distinct.numpy()
    
    return ICUSDataset(isforget, descriptions, shared, distinct, seencids)

def load_semantics(semants_loc: str, classes_loc: str = None, unseen_loc: str = None) -> ndarray:
    '''returns semantics in shape (nclasses, nattribs)'''
    if classes_loc is None or unseen_loc is None: return npyload(semants_loc).T
    assert classes_loc is not None and unseen_loc is not None, f"received only one not None value on unseen_loc and classes_loc, both or no one are required"
    unseencids = load_txt(unseen_loc)
    allcids = sorted([r["cid"] for _,r in read_csv(classes_loc, sep=";", names=["cid", "name"]).iterrows()])
    seencids = sorted([cid for cid in allcids if cid not in unseencids])
    seen_indexes = [allcids.index(scid) for scid in seencids]

    return npyload(semants_loc).T[seen_indexes]

def load_txt(file) -> list[int]:
    with open(file, "r") as fr: return [int(data) for data in fr.read().split(";")]

def save_txt(file, data: list[int]) -> None:
    with open(file, "w") as fw: return fw.write(';'.join([str(d) for d in data]))