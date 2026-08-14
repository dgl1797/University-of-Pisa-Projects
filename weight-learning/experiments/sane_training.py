import sys, os, torch
from pathlib import Path
from wandb import Image as WBImage
from argparse import ArgumentParser

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from src.utils.config_manager import use_config
from src.utils.schemas.sane import SANE_CONF
from src.implementations.sane import SANE_BASE
from src.implementations.classification import ClassificationTask
from src.datasets.weights.tokenized_model_weights import TokenizedModelWeightDataset, AugmentedTokenizedWeightDataset
from src.datasets.images.generic_image import GenericImagesDataset
from src.datasets.images.helpers import load_rows_from_csv, load_txt
from src.models.autoencoders.sane import SANE
from src.models.classifiers.resnet18 import ResNet18
from src.utils.plots import layers_histogram

@use_config("sane", SANE_CONF, multiruns={
    "training.stride": [4,2,1], "ae.nblocks": [2,1], "ae.nhead": [2,1], "ae.embedding_dim": [512, 256]
})
def train_sane(conf: SANE_CONF, name: str, mode: str = "base"):
    # Sane Training Setup
    stride = conf.training.windowsize // conf.training.stride
    runname = f"sane_{name}.sws{conf.training.stride}_b{conf.ae.nblocks}_e{conf.ae.embedding_dim}_h{conf.ae.nhead}"
    trainer = SANE_BASE(conf, log_run=runname, device=conf.device)

    target_ckpt = torch.load(Path(conf.paths.trainckpt_loc))
    nclasses = target_ckpt["model.fc.weight"].shape[0]
    wsize = conf.training.windowsize
    trainset = None
    if mode == "base": trainset = TokenizedModelWeightDataset(target_ckpt, trainer.tokenizer, wsize, stride=stride)
    elif mode == "augment":
        imgtst = GenericImagesDataset(load_rows_from_csv(conf.paths.testimges_loc), load_txt(conf.paths.testimgfilter), "default")
        imgloader = torch.utils.data.DataLoader(imgtst, 32, False, num_workers=conf.nworkers, persistent_workers=True)
        trainset = AugmentedTokenizedWeightDataset(
            target_ckpt, nclasses, trainer.tokenizer, wsize, nrandom=conf.training.nrandom, stride=stride, device=conf.device, 
            testloader=imgloader, step=5e-2, performance_loss_tolerance=1e-1 
        )
    trainloader = torch.utils.data.DataLoader(trainset, conf.training.batchsize, True, num_workers=conf.nworkers, persistent_workers=True)
    
    idim = trainset.tdim() if conf.ae.input_dim == 0 else conf.ae.input_dim
    sane_model = SANE(
        idim=idim, 
        edim=conf.ae.embedding_dim, 
        nhead=conf.ae.nhead, 
        nblocks=conf.ae.nblocks, 
        latdim=conf.ae.latent_dim, 
        wsize=conf.training.windowsize, 
        max_positions=conf.ae.max_positions
    )

    # Best State Retrieval / Training
    try:
        location = next(trainer.store_location.joinpath("best").glob("ckpt_*.pt"))
        best_checkpoint = torch.load(location, weights_only=False)["state_dict"]
        sane_model.load_state_dict(best_checkpoint)
    except StopIteration:
        trainmetric = trainer.setup_training(sane_model, trainloader, None).train()
        trainer.compare_and_store(trainmetric, sane_model.state_dict(), range(len(trainset)), mode="min")
    # sane model at this point has trained checkpoint
    
    # Reconstruction
    test_checkpoint = torch.load(Path(conf.paths.testchkpt_loc))
    # testset always built with stride = windowsize
    testset = TokenizedModelWeightDataset(test_checkpoint, trainer.tokenizer, conf.training.windowsize)
    testloader = torch.utils.data.DataLoader(testset, conf.training.batchsize, False, num_workers=conf.nworkers, persistent_workers=True)
    recontokens, positions = trainer.setup_test(sane_model, testloader).test()
    injected_checkpoint = trainer.tokenizer.inject_tokens(test_checkpoint, recontokens, positions)
    ckptloc = trainer.store_location.joinpath("injections")
    ckptloc.mkdir(777, parents=True, exist_ok=True)
    ckptloc = ckptloc.joinpath("injected.pt")
    ckptloc.unlink(missing_ok=True); torch.save(injected_checkpoint, ckptloc)
    
    # classification task preparation
    imgds = GenericImagesDataset(load_rows_from_csv(conf.paths.testimges_loc), load_txt(conf.paths.testimgfilter), transform="default")
    imageloader = torch.utils.data.DataLoader(imgds, conf.training.batchsize, False, num_workers=conf.nworkers, persistent_workers=True)
    nclasses = len(imgds.cids_map)
    classificationtask = ClassificationTask("sane_injected_test", nclasses, conf.device)
    classifier_network = ResNet18(len(imgds.cids_map), False)
    injected_checkpoint = torch.load(ckptloc)
    original_checkpoint = torch.load(Path("checkpoints", "resnet18_awa2.pt"))
    
    # classification task
    classifier_network.load_state_dict(original_checkpoint)
    original_metrics = classificationtask(classifier_network, imageloader, "Origin Eval")
    classifier_network.load_state_dict(injected_checkpoint)
    injected_metrics = classificationtask(classifier_network, imageloader, "Inject Eval")

    # layer by layer histogram plotting
    if trainer.logger:
        print("Logging...")
        for idx, layer, figure, mse in layers_histogram(test_checkpoint, injected_checkpoint):
            if idx != -1: trainer.logger.log({f"{idx}.{layer}/plot": WBImage(figure), f"MSEs/{idx}.{layer}": mse})
            else: trainer.logger.log({f"Test/{layer}": WBImage(figure)}) # layer becomes the plot's title
        
        trainer.logger.log({f"Test/Original_{k}": v for k,v in original_metrics.items()})
        trainer.logger.log({f"Test/Injected_{k}": v for k,v in injected_metrics.items()})
        trainer.logger.finish()

    return

if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument('-n', '--name', type=str, required=True, help="name of the experiment")
    parser.add_argument('-t', '--type', choices=["base", "augment"], default='base', help="decides if to execute the experiment with augmented dataset or with base one")
    args = vars(parser.parse_args())
    train_sane(name=args['name'], mode=args["type"])