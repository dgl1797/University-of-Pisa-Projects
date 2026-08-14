import wandb, os, argparse, shutil

def upload_files(project_root: str, run_name: str = "project-files", data_root: str = "data", exclusion_list: list[str] = ["test"]):
    out_root = os.path.join(project_root, "out")
    apy_root = os.path.join(project_root, data_root, "APY_Data")
    cub_root = os.path.join(project_root, data_root, "CUB_Data")
    awa2_root = os.path.join(project_root, data_root, "AWA2_Data")
    sun_root = os.path.join(project_root, data_root, "SUN_Data")
    filter_list = lambda src,names: [name for name in names if name.startswith(tuple(exclusion_list)) and os.path.isdir(os.path.join(src, name))]

    api = wandb.Api()
    for run in api.runs("dgl1797/Thesis"):
        if run.name.lower().replace(" ", "-") == run_name: run.delete()

    wandb.init("dgl1797", "Thesis", name=run_name, group="files", dir=project_root)
    # Output directory
    shutil.copytree(os.path.join(out_root), os.path.join(wandb.run.dir, "outputs"), ignore=filter_list)

    # image splits
    shutil.copytree(os.path.join(apy_root, "image_splits"), os.path.join(wandb.run.dir, "outputs", "image_splits", "apy_splits"), ignore=filter_list)
    shutil.copytree(os.path.join(cub_root, "image_splits"), os.path.join(wandb.run.dir, "outputs", "image_splits", "cub_splits"), ignore=filter_list)
    shutil.copytree(os.path.join(awa2_root, "image_splits"), os.path.join(wandb.run.dir, "outputs","image_splits",  "awa2_splits"), ignore=filter_list)
    shutil.copytree(os.path.join(sun_root, "image_splits"), os.path.join(wandb.run.dir, "outputs", "image_splits", "sun_splits"), ignore=filter_list)

    # class splits
    shutil.copytree(os.path.join(apy_root, "class_splits"), os.path.join(wandb.run.dir, "outputs", "class_splits", "apy_splits"))
    shutil.copytree(os.path.join(cub_root, "class_splits"), os.path.join(wandb.run.dir, "outputs", "class_splits", "cub_splits"))
    shutil.copytree(os.path.join(awa2_root, "class_splits"), os.path.join(wandb.run.dir, "outputs", "class_splits", "awa2_splits"))
    shutil.copytree(os.path.join(sun_root, "class_splits"), os.path.join(wandb.run.dir, "outputs", "class_splits", "sun_splits"))

    # embeddings
    shutil.copytree(os.path.join(apy_root, "embeddings"), os.path.join(wandb.run.dir, "outputs", "embeddings", "apy_embeds"))
    shutil.copytree(os.path.join(cub_root, "embeddings"), os.path.join(wandb.run.dir, "outputs", "embeddings", "cub_embeds"))
    shutil.copytree(os.path.join(awa2_root, "embeddings"), os.path.join(wandb.run.dir, "outputs", "embeddings", "awa2_embeds"))
    shutil.copytree(os.path.join(sun_root, "embeddings"), os.path.join(wandb.run.dir, "outputs", "embeddings", "sun_embeds"))

    # upload
    wandb.save(os.path.join(wandb.run.dir, "outputs", "**", "*.pt"), base_path=wandb.run.dir)
    wandb.save(os.path.join(wandb.run.dir, "outputs", "**", "*.npy"), base_path=wandb.run.dir)
    wandb.save(os.path.join(wandb.run.dir, "outputs", "**", "*.mat"), base_path=wandb.run.dir)
    wandb.save(os.path.join(wandb.run.dir, "outputs", "**", "*.csv"), base_path=wandb.run.dir)
    wandb.save(os.path.join(wandb.run.dir, "outputs", "**", "*.db"), base_path=wandb.run.dir)

    wandb.finish()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("-rn", "--run-name", type=str, default="project-files", help="specify the name of the run where to upload the files, it will delete any existing run with that name")
    parser.add_argument("-dr", "--data-root", type=str, default="data", help="allows to specify different data location for splits upload")
    parser.add_argument("-el", "--exclusion-list", type=str, nargs="+", default=["test"], help="allows to specify an exclusion list, the outputs corresponding to that base split won't be uploaded. anything named 'test' won't be uploaded anyway")
    args = vars(parser.parse_args())
    run_name = args["run_name"]
    dr = args["data_root"]
    el = args["exclusion_list"] + ["test"]
    project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
    upload_files(project_root=project_root, run_name=run_name, data_root=dr, exclusion_list=el)
    shutil.rmtree(os.path.join(project_root, "wandb"), ignore_errors=True)
