import os, argparse, numpy as np, pandas as pd
from transformers import BertModel, BertTokenizer
from torch import no_grad, Tensor
from tqdm import tqdm

import helpers as shelp

class AttributeMaker():
    def __init__(self, data_root: str, filename: str, dsname: str):
        self.file_loc = os.path.join(data_root, f"{dsname.upper()}_Data", "embeddings", f"{filename}.npy")
        if forced and os.path.exists(self.file_loc): os.remove(self.file_loc)
        assert not os.path.exists(self.file_loc), f"{self.file_loc} already exists, use --forced to replace it" 
        os.makedirs(os.path.dirname(self.file_loc), 777, exist_ok=True)
        self.dataroot = data_root
        self.filename = filename
        self.ds = dsname
    
    def extract(self):
        project_classlist = pd.read_csv(os.path.join(self.dataroot, f"{self.ds.upper()}_Data", "classes.csv"), sep=";", names=["cid", "cn"])
        project_classlist = [r["cn"] for _,r in project_classlist.iterrows()]
        from zipfile import ZipFile; import io; from scipy.io import loadmat
        archive_loc = os.path.join(self.dataroot, "archives", "splits.zip")
        with ZipFile(archive_loc, "r") as zarch:
            filename = f"xlsa17/data/{self.ds.upper()}/att_splits.mat"
            with zarch.open(filename) as fr:
                mat_content = io.BytesIO(fr.read())
                data = loadmat(mat_content)
                attrib_vec = data['att']
                classes: list[str] = [str(cn[0]) for cn in data['allclasses_names'].flatten()]
                ptof_map = [classes.index(pcn) for pcn in project_classlist]
                sorted_vec = attrib_vec[:, ptof_map]
                np.save(self.file_loc, sorted_vec)
                original_vec = data['original_att']
                sorted_original_vec = original_vec[:, ptof_map]
                np.save(os.path.join(*os.path.split(self.file_loc)[:-1], f"original_{self.filename}.npy"), sorted_original_vec)
    
    def binary(self):
        project_classlist = pd.read_csv(os.path.join(self.dataroot, f"{self.ds.upper()}_Data", "classes.csv"), sep=";", names=["cid", "cn"])
        project_classlist = [r["cn"] for _,r in project_classlist.iterrows()]
        from zipfile import ZipFile; import io; from scipy.io import loadmat
        archive_loc = os.path.join(self.dataroot, "archives", "splits.zip")
        with ZipFile(archive_loc, "r") as zarch:
            if self.ds == "awa2":
                filename = f"xlsa17/data/{self.ds.upper()}/binaryAtt_splits.mat"
                with zarch.open(filename) as fr:
                    mat_content = io.BytesIO(fr.read())
                    data = loadmat(mat_content)
                    attrib_vec = data['att']
                    classes: list[str] = [str(cn[0]) for cn in data['allclasses_names'].flatten()]
                    ptof_map = [classes.index(pcn) for pcn in project_classlist]
                    sorted_attrib_vec = attrib_vec[:, ptof_map]
                    np.save(os.path.join(*os.path.split(self.file_loc)[:-1], f"{self.filename}.npy"), sorted_attrib_vec)
            else:
                filename = f"xlsa17/data/{self.ds.upper()}/att_splits.mat"
                with zarch.open(filename) as fr:
                    mat_content = io.BytesIO(fr.read())
                    data = loadmat(mat_content)
                    original_vec: np.ndarray = data['original_att']
                    binary_array: np.ndarray = np.where(original_vec > 0, 1, 0)
                    classes: list[str] = [str(cn[0]) for cn in data['allclasses_names'].flatten()]
                    ptof_map = [classes.index(pcn) for pcn in project_classlist]
                    sorted_binattrs = binary_array[:, ptof_map]
                    np.save(os.path.join(*os.path.split(self.file_loc)[:-1], f"{self.filename}.npy"), sorted_binattrs)
    
    def bert(self):
        project_classlist = pd.read_csv(os.path.join(self.dataroot, f"{self.ds.upper()}_Data", "classes.csv"), sep=";", names=["cid", "cn"])
        project_classlist = sorted([r for _,r in project_classlist.iterrows()], key=lambda row: row["cid"])
        formatted_classnames = shelp.format_classname([r["cn"] for r in project_classlist], self.ds)

        tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
        model = BertModel.from_pretrained('bert-base-uncased')

        print(f"{self.ds.upper()} - Start:")
        bert_descriptions = []
        for cn in formatted_classnames:
            wiki_description = None # shelp.get_wiki_description(cn)
            wiki_description = cn if wiki_description is None or wiki_description == "" else wiki_description
            bert_descriptions.append(wiki_description)

        print("Computing BERT embeddings")
        encodings = tokenizer.batch_encode_plus(bert_descriptions, padding=True, truncation=True, return_tensors="pt", add_special_tokens=True)
        with no_grad():
            outputs = model(input_ids=encodings['input_ids'], attention_mask=encodings['attention_mask'])
            # taking the [CLS] special token following https://huggingface.co/google-bert/bert-base-uncased
            embeddings: Tensor = outputs.last_hidden_state[:, 0, :].T
            print(f"Extracted CLS token: {embeddings.shape}")
        
        print("Converting embedding in Numpy binary file")
        np.save(os.path.join(*os.path.split(self.file_loc)[:-1], f"{self.filename}.npy"), embeddings.numpy())
        print(f"{self.ds.upper()} - Done.")


if __name__ == '__main__':
    available_methods: list[str] = [
        "extract", "binary", "bert"
    ]
    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--datasets", nargs='+', type=str, choices=["cub","awa2","sun","apy"], required=True, help="attributes of selected datasets to be extracted")
    parser.add_argument("-m", "--method", type=str, choices=available_methods, default="extract", help="method of extraction, default is 'extract'")
    parser.add_argument("-n", "--name", type=str, default="attribs", help="name of the produced file in <dataset_root>/class_splits default is 'attribs'")
    parser.add_argument("-f", "--forced", action='store_true', default=False, help="if a file with the same name is found, this option will delete it and replace with a new split")
    parser.add_argument("-r", "--root", type=str, default="data", help="changes the data root location, default is 'data'")
    args = vars(parser.parse_args())
    dsnames = args["datasets"]
    forced = args["forced"]

    for dsname in dsnames: getattr(AttributeMaker(args["root"], args["name"], dsname), args["method"])()