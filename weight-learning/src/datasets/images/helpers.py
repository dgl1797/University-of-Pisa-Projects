from pandas import read_csv

def load_txt(txtloc: str):
    with open(txtloc, "r") as fr: return [int(data) for data in fr.read().split(";")]

def load_rows_from_csv(csvloc: str, sep: str = ";", names: str = ["cid", "path"]):
    return [r for _,r in read_csv(csvloc, sep=sep, names=names).iterrows()]