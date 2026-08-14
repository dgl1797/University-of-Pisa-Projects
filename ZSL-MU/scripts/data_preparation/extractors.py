import os, tarfile
from zipfile import ZipFile
from pandas import read_csv
from io import BytesIO
from PIL import Image
from scipy.io import loadmat

def extract_awa2(aloc: str, dloc: str, verbose: bool = False, forced: bool = False):
    images_path= "Animals_with_Attributes2/JPEGImages"
    classnames = "Animals_with_Attributes2/classes.txt"
    attributes = "Animals_with_Attributes2/predicates.txt"
    data_archive = os.path.join(aloc, "awa2.zip")
    images_loc = os.path.join(dloc, "images")
    os.makedirs(images_loc, 777)
    
    with ZipFile(data_archive) as zarch:
        # classes
        with zarch.open(classnames, "r") as fr: class_maps = {row["cn"]:row["cid"] for _,row in read_csv(fr, sep="\t", names=["cid", "cn"]).iterrows()}
        # attribs
        with zarch.open(attributes, "r") as fr: attrb_maps = {row["an"]:row["aid"] for _,row in read_csv(fr, sep="\t", names=["aid", "an"]).iterrows()}
        # images
        image_lines = []
        for imagename in [name for name in zarch.namelist() if name.startswith(images_path) and name.endswith((".jpg",".jpeg"))]:
            class_name = imagename.split("/")[-2]; class_id = class_maps[class_name]; imname = imagename.split("/")[-1]
            image_path = os.path.join(images_loc, class_name, imname); image_lines.append(f"{class_id};{image_path}\n")
            if verbose: print(f"Extracting {imagename} in {image_path}")
            os.makedirs(os.path.dirname(image_path), 777, exist_ok=True)
            with zarch.open(imagename, "r") as frb, open(image_path, "wb") as fwb:
                fwb.write(frb.read())
    
    with open(os.path.join(dloc, "classes.csv"), "w") as fw:
        if verbose: print("compiling classes.csv")
        fw.writelines([f"{v};{k}\n" for k,v in class_maps.items()])
    with open(os.path.join(dloc, "images.csv"), "w") as fw:
        if verbose: print("compiling images.csv")
        fw.writelines(image_lines)
    with open(os.path.join(dloc, "attribs.csv"), "w") as fw:
        if verbose: print("compiling attribs.csv")
        fw.writelines([f"{v};{k}\n" for k,v in attrb_maps.items()])
    return None

def extract_apy(aloc: str, dloc: str, verbose: bool = False, forced: bool = False) :
    images_path= "VOCdevkit/VOC2008/JPEGImages"
    classnames = "attribute_data/class_names.txt"
    attributes = "attribute_data/attribute_names.txt"
    apascal_train = "attribute_data/apascal_train.txt"
    apascal_test = "attribute_data/apascal_test.txt"
    ayahoo_test = "attribute_data/ayahoo_test.txt"
    pascal_arch = os.path.join(aloc, "apascal.tar")
    yahoo_arch = os.path.join(aloc, "ayahoo.tar.gz")
    attr_archive = os.path.join(aloc, "apascal_attrib.tar.gz")
    images_loc = os.path.join(dloc, "images")
    os.makedirs(images_loc, 777)

    # info reading
    info_lines: dict[str, list[tuple[str, tuple[int, int, int, int]]]] = {}
    with tarfile.open(attr_archive) as tarch:
        with tarch.extractfile(classnames) as fr: class_maps = {ln.decode('utf-8').replace('\n',''): idx for idx,ln in enumerate(fr.readlines())}
        with tarch.extractfile(attributes) as fr: attrb_maps = {ln.decode('utf-8').replace('\n',''): idx for idx,ln in enumerate(fr.readlines())}
        if verbose: print("compiling apascal_train info")
        with tarch.extractfile(apascal_train) as fr: 
            for l in [line.decode('utf-8').replace('\n','') for line in fr.readlines()]:
                args = l.split(" ")[0:6]
                if args[0] not in info_lines: info_lines[args[0]] = [(args[1], (int(args[2]), int(args[3]), int(args[4]), int(args[5])))]
                else: info_lines[args[0]].append((args[1], (int(args[2]), int(args[3]), int(args[4]), int(args[5]))))
        if verbose: print("compiling apascal_test info")
        with tarch.extractfile(apascal_test) as fr:
            for l in [line.decode('utf-8').replace('\n','') for line in fr.readlines()]:
                args = l.split(" ")[0:6]
                if args[0] not in info_lines: info_lines[args[0]] = [(args[1], (int(args[2]), int(args[3]), int(args[4]), int(args[5])))]
                else: info_lines[args[0]].append((args[1], (int(args[2]), int(args[3]), int(args[4]), int(args[5]))))
        if verbose: print("compiling ayahoo_test info")
        with tarch.extractfile(ayahoo_test) as fr:
            for l in [line.decode('utf-8').replace('\n','') for line in fr.readlines()]:
                args = l.split(" ")[0:6]
                if args[0] not in info_lines: info_lines[args[0]] = [(args[1], (int(args[2]), int(args[3]), int(args[4]), int(args[5])))]
                else: info_lines[args[0]].append((args[1], (int(args[2]), int(args[3]), int(args[4]), int(args[5]))))
    
    image_lines = []
    with tarfile.open(pascal_arch) as tarch:
        for imfile in info_lines.keys():
            if not imfile in [m.name.split("/")[-1] for m in tarch.getmembers()]: continue
            with tarch.extractfile(f"{images_path}/{imfile}") as fr: 
                bincontent = BytesIO(fr.read())
                with Image.open(bincontent) as img:
                    for idx, crop in enumerate(info_lines[imfile]):
                        classname = crop[0]; bbox = crop[1]; img_pth = os.path.join(images_loc, classname, f"{imfile.split('.')[0]}_c{idx}.jpg")
                        if verbose: print(f"Extracting apascal {images_path}/{imfile} into {img_pth}")
                        os.makedirs(os.path.dirname(img_pth), 777, exist_ok=True)
                        image_lines.append(f"{class_maps[classname]};{img_pth}\n")
                        img.crop(bbox).save(img_pth)
    images_path = "ayahoo_test_images"
    with tarfile.open(yahoo_arch) as tarch:
        for imfile in info_lines.keys():
            if not f"{images_path}/{imfile}" in [m.name for m in tarch.getmembers()]: continue
            with tarch.extractfile(f"{images_path}/{imfile}") as fr:
                bincontent = BytesIO(fr.read())
                with Image.open(bincontent) as img:
                    for idx,crop in enumerate(info_lines[imfile]):
                        classname = crop[0]; bbox = crop[1]; img_pth = os.path.join(images_loc, classname, f"{imfile.split('.')[0]}_c{idx}.jpg")
                        if verbose: print(f"Extracting ayahoo {images_path}/{imfile} into {img_pth}")
                        os.makedirs(os.path.dirname(img_pth), 777, exist_ok=True)
                        image_lines.append(f"{class_maps[classname]};{img_pth}\n")
                        if bbox[0] == bbox[2] or bbox[1] == bbox[3]: img.save(img_pth)
                        else: img.crop(bbox).save(img_pth)

    with open(os.path.join(dloc, "classes.csv"), "w") as fw:
        if verbose: print("compiling classes.csv")
        fw.writelines([f"{v};{k}\n" for k,v in class_maps.items()])
    with open(os.path.join(dloc, "images.csv"), "w") as fw:
        if verbose: print("compiling images.csv")
        fw.writelines(image_lines)
    with open(os.path.join(dloc, "attribs.csv"), "w") as fw:
        if verbose: print("compiling attribs.csv")
        fw.writelines([f"{v};{k}\n" for k,v in attrb_maps.items()])
    return None

def extract_sun(aloc: str, dloc: str, verbose: bool = False, forced: bool = False):
    images_path = "images"
    images_list = "SUNAttributeDB/images.mat"
    attributes = "SUNAttributeDB/attributes.mat"
    data_archive = os.path.join(aloc, "sun.tar.gz")
    attr_archive = os.path.join(aloc, "sun_attrib.tar.gz")
    images_loc = os.path.join(dloc, "images")
    os.makedirs(images_loc, 777)
    
    with tarfile.open(attr_archive) as tarch:
        with tarch.extractfile(images_list) as fr: sunattr_imglist = [str(item[0]) for item in loadmat(fr)['images'].flatten().tolist()]
        with tarch.extractfile(attributes) as fr: sunattr_attlist = [str(item[0]) for item in loadmat(fr)['attributes'].flatten().tolist()]
    class_maps = sorted(list(set(['_'.join(pth.split("/")[-len(pth.split("/"))+1:-1]) for pth in sunattr_imglist])))
    attrb_maps = sorted([att for att in sunattr_attlist])
    class_maps = {cn:idx for idx,cn in enumerate(class_maps)}
    attrb_maps = {cn:idx for idx,cn in enumerate(attrb_maps)}

    image_lines = []
    with tarfile.open(data_archive) as tarch:
        for img in sunattr_imglist:
            classname = '_'.join(img.split("/")[-len(img.split("/"))+1:-1]); image = img.split("/")[-1]; cid = class_maps[classname]
            imgpath = os.path.join(images_loc, classname, image); image_lines.append(f"{cid};{imgpath}\n")
            if verbose: print(f"Extracting {images_path}/{img} into {imgpath}")
            os.makedirs(os.path.dirname(imgpath), 777, exist_ok=True)
            with tarch.extractfile(f"{images_path}/{img}") as fr, open(imgpath, "wb") as fwb:
                fwb.write(fr.read())
    
    with open(os.path.join(dloc, "classes.csv"), "w") as fw:
        if verbose: print("compiling classes.csv")
        fw.writelines([f"{v};{k}\n" for k,v in class_maps.items()])
    with open(os.path.join(dloc, "images.csv"), "w") as fw:
        if verbose: print("compiling images.csv")
        fw.writelines(image_lines)
    with open(os.path.join(dloc, "attribs.csv"), "w") as fw:
        if verbose: print("compiling attribs.csv")
        fw.writelines([f"{v};{k}\n" for k,v in attrb_maps.items()])
    return None

def extract_cub(aloc: str, dloc: str, verbose: bool = False, forced: bool = False):
    images_path= "CUB_200_2011/images"
    classnames = "CUB_200_2011/classes.txt"
    attributes = "attributes.txt"
    data_archive = os.path.join(aloc, "cub.tar.gz")
    images_loc = os.path.join(dloc, "images")
    os.makedirs(images_loc, 777)
    
    with tarfile.open(data_archive) as tarch:
        # classes
        with tarch.extractfile(classnames) as fr: class_maps = {row["cn"]:row["cid"] for _,row in read_csv(fr, sep=" ", names=["cid", "cn"]).iterrows()}
        # attribs
        with tarch.extractfile(attributes) as fr: attrb_maps = {row["an"]:row["aid"] for _,row in read_csv(fr, sep=" ", names=["aid", "an"]).iterrows()}
        # images
        image_lines = []
        for imagename in [name.name for name in tarch.getmembers() if name.name.startswith(images_path) and name.name.endswith((".jpg",".jpeg"))]:
            class_name = imagename.split("/")[-2]; class_id = class_maps[class_name]; imname = imagename.split("/")[-1]
            image_path = os.path.join(images_loc, class_name, imname); image_lines.append(f"{class_id};{image_path}\n")
            if verbose: print(f"Extracting {imagename} in {image_path}")
            os.makedirs(os.path.dirname(image_path), 777, exist_ok=True)
            with tarch.extractfile(imagename) as frb, open(image_path, "wb") as fwb:
                fwb.write(frb.read())
    
    with open(os.path.join(dloc, "classes.csv"), "w") as fw:
        if verbose: print("compiling classes.csv")
        fw.writelines([f"{v};{k}\n" for k,v in class_maps.items()])
    with open(os.path.join(dloc, "images.csv"), "w") as fw:
        if verbose: print("compiling images.csv")
        fw.writelines(image_lines)
    with open(os.path.join(dloc, "attribs.csv"), "w") as fw:
        if verbose: print("compiling attribs.csv")
        fw.writelines([f"{v};{k}\n" for k,v in attrb_maps.items()])
    return None