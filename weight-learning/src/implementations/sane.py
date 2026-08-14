import os, sys, torch
from copy import deepcopy
from pathlib import Path
from tqdm import tqdm
from math import inf
import wandb
import wandb.wandb_run

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
from src.utils.schemas.sane import SANE_CONF
from src.models.autoencoders.sane import SANE
from src.datasets.tokenizer import Tokenizer
from src.losses.GammaContrastReconLoss import MaskedReconLoss, GammaContrastReconLoss
from src.augmentations.pipelines import WindowPipeline
from src.utils.loggers import get_wandb

class SANE_BASE():
    '''
    Basic SANE training environment, uses configuration to instantiate the environment without model and loaders
    this basic version trains SANE to reconstruct the original checkpoint by only looking at the MSE loss
    ### Usage
    ```python
        trainer = SANE_BASE(config, log_run="sane.basic_training", device="cuda")
        training_metrics = trainer.setup_training(sane_model, trainingloader, validloader).train()
        test_metrics = trainer.setup_test(sane_model, testloader).test()
    ```
    '''
    def __init__(self, conf: SANE_CONF, log_run: str = None, device: str = "cpu"):
        self.device = device
        self.conf = conf
        self.tokenizer = Tokenizer(tokensize=conf.ae.input_dim, use_masks=True)
        self.scaler = torch.GradScaler(device, enabled=True)
        self.store_location = Path("out", log_run if log_run is not None else "sane")
        self.store_location.mkdir(777, parents=True, exist_ok=True)
        self.criterion = MaskedReconLoss(self.conf.training.reduction)
        self.logger = None
        if log_run: run_group, run_name = log_run.split(".")
        if log_run: self.logger: wandb.wandb_run.Run = get_wandb(name=run_name, group=run_group)
        # no gradient clipping as on main repo it is disabled by config
        return

    def __init_optimizer(self):
        trainable_params = {pn: p for pn,p in self.target.named_parameters() if p.requires_grad}
        decay_params = [p for _,p in trainable_params.items() if p.dim() >= 2]
        nodec_params = [p for _,p in trainable_params.items() if p.dim() < 2]
        optim_groups = [
            {"params": decay_params, "weight_decay": self.conf.optimizer.wd},
            {"params": nodec_params, "weight_decay": 0.0}
        ]
        totsteps = self.conf.training.nepochs * len(self.trainloader)
        self.optimizer = torch.optim.AdamW(params=optim_groups, lr=self.conf.optimizer.lr)
        self.scheduler = torch.optim.lr_scheduler.OneCycleLR(self.optimizer, self.conf.optimizer.lr, totsteps)
        return
    
    def setup_training(self, target: SANE, trainloader: torch.utils.data.DataLoader, validloader: torch.utils.data.DataLoader):
        self.target = target
        self.trainloader = trainloader
        self.validloader = validloader
        self.__init_optimizer()
        return self
    
    def setup_test(self, target: SANE, testloader: torch.utils.data.DataLoader):
        self.target = target
        self.testloader = testloader
        return self

    @torch.no_grad()
    def validate(self, triplet: list[torch.Tensor]) -> float:
        with torch.autocast(self.device, enabled=True):
            z1,y1,zp1 = self.target.forward(triplet[0], triplet[2])
            loss: torch.Tensor = self.criterion.forward(y1,triplet[0], triplet[1])
        return loss.item()
    
    def train_step(self, triplet: list[torch.Tensor]):
        with torch.autocast(self.device, enabled=True):
            # here goes the augmentation to generate two views of the triplet (t, m, p)
            self.optimizer.zero_grad(set_to_none=True)
            z1,y1,zp1 = self.target.forward(triplet[0], triplet[2])
            # here goes the forward of the second view to enable contrastive loss
            loss: torch.Tensor = self.criterion.forward(y1, triplet[0], triplet[1])
        self.scaler.scale(loss).backward()
        self.scaler.step(self.optimizer)
        self.scaler.update()
        self.scheduler.step()
        return loss.item()

    @torch.no_grad()
    def test_step(self, triplet: list[torch.Tensor]):
        with torch.autocast(self.device, enabled=True):
            z,y,zp = self.target.forward(triplet[0], triplet[2])
        return y.to("cpu")
    
    @torch.no_grad()
    def test(self):
        self.target.eval().to(self.device)
        reconwindows: list[torch.Tensor] = []
        positions: list[torch.Tensor] = []
        for triplet in tqdm(self.testloader, desc="Testing"):
            triplet = [t.to(self.device) for t in triplet]
            recon = self.test_step(triplet).to("cpu")
            reconwindows.append(recon.reshape(recon.shape[0]*recon.shape[1], -1))
            positions.append(triplet[2].to("cpu").reshape(triplet[2].shape[0]*triplet[2].shape[1], -1))
            del triplet
        return torch.cat(reconwindows, dim=0), torch.cat(positions, dim=0)

    def train(self):
        self.target.to(self.device); best_metric: float = inf; patience = 0; best_state = deepcopy(self.target.state_dict())
        for epoch in range(self.conf.training.nepochs):
            print(f"best metric: {best_metric:e}")
            trainloss = 0; self.target.train()
            for triplet in tqdm(self.trainloader, desc=f"Training {epoch+1}/{self.conf.training.nepochs}"):
                triplet = [t.to(self.device) for t in triplet] # t,m,p
                trainloss += self.train_step(triplet)
            trainloss /= len(self.trainloader)
            self.logger.log({"Train/train-loss": trainloss, "Train/epoch": epoch})
            
            if self.validloader:
                validloss = 0; self.target.eval()
                for triplet in tqdm(self.validloader, desc=f"Validating {epoch+1}/{self.conf.training.nepochs}"):
                    triplet = [t.to(self.device) for t in triplet]
                    validloss += self.validate(triplet)
                validloss /= len(self.validloader)
                self.logger.log({"Train/valid-loss": validloss, "Train/epoch": epoch})
            else: validloss = trainloss

            if validloss < best_metric: patience = 0; best_metric = validloss; best_state = deepcopy(self.target.state_dict())
            elif patience+1 == self.conf.training.patience: break
            else: patience += 1
        
        self.target.to("cpu").load_state_dict(best_state)
        return best_metric
    
    def kfold(self, target: SANE, trainset: torch.utils.data.Dataset, nfolds: int = 10):
        folds = [len(trainset)//nfolds]*(nfolds-1) + [len(trainset)//nfolds + len(trainset)%nfolds]
        for i in range(len(folds)): folds[i] = (0, folds[i]) if i == 0 else (folds[i-1][1], folds[i-1][1]+folds[i])
        indexes = torch.randperm(len(trainset)).tolist()
        # folds -> [(fold_sdx, fold_edx)]
        avg_metric = 0
        for vfold in range(len(folds)):
            target_copy = deepcopy(target)
            train_indexes = sorted([el for i,f in enumerate(folds) if i != vfold for el in indexes[f[0]:f[1]]])
            valid_indexes = sorted([el for el in indexes[folds[vfold][0]: folds[vfold][1]]])
            self.setup_training(
                target = target_copy,
                trainloader = torch.utils.data.DataLoader(
                    torch.utils.data.Subset(trainset, train_indexes),
                    batch_size=self.conf.training.batchsize, shuffle=True, num_workers=self.conf.nworkers, persistent_workers=True
                ),
                validloader = torch.utils.data.DataLoader(
                    torch.utils.data.Subset(trainset, valid_indexes),
                    batch_size=self.conf.training.batchsize, shuffle=False, num_workers=self.conf.nworkers, persistent_workers=True
                )
            )
            fold_best_metric = self.train()
            self.compare_and_store(fold_best_metric, target_copy.state_dict(), train_indexes)
            avg_metric += fold_best_metric
        avg_metric /= nfolds
        target.load_state_dict(self.best_state()["state_dict"])
        return avg_metric
    
    # file management
    def best_state(self) -> tuple[float, dict[str, torch.nn.Module], list[int]]:
        return torch.load(next(self.store_location.joinpath("best").glob("ckpt_*.pt")))
    
    def compare_and_store(self, new_metric: float, new_state: dict[str, torch.nn.Module], train_indexes: list[int], mode: str = "min"):
        bestlocation = self.store_location.joinpath("best")
        try:
            bestlocation.mkdir(777, parents=True, exist_ok=True)
            best_ckpt = next(bestlocation.glob("ckpt_*.pt"))
        except StopIteration:
            best_ckpt = None
        
        if best_ckpt is not None:
            best_metric = float(".".join(best_ckpt.parts[-1].split(".")[:-1]).split("_")[1]) # cuts off .pt and ckpt_
            
            # no change needed
            if best_metric >= new_metric and mode == "max": return
            if best_metric <= new_metric and mode == "min": return

        # change needed
        if best_ckpt is not None: best_ckpt.unlink(missing_ok=True)
        torch.save({"best_metric": new_metric, "state_dict": new_state, "train_indexes": train_indexes}, bestlocation.joinpath(
            f"ckpt_{new_metric:e}.pt"
        ))

class SANE_WAUGMENT(SANE_BASE):
    '''
    Inherits from SANE_BASE but introduces also the contrastive Normalized Temperature Cross Entropy (NT-Xent) loss between the
    projected embeddings to align projections of different augmentations to the same point
    '''
    def __init__(self, conf: SANE_CONF, window_augmentation: WindowPipeline = None, log_run: str = None, device = "cpu"):
        super().__init__(conf, device)
        self.store_location = Path("out", log_run if log_run is not None else "sane_augment")
        self.store_location.mkdir(777, parents=True, exist_ok=True)
        self.criterion = GammaContrastReconLoss(
            conf.training.gamma, conf.training.reduction, conf.training.temperature
        )
        self.waugment = window_augmentation
    
    @torch.no_grad()
    def validate(self, triplet: list[torch.Tensor]) -> float:
        self.target.eval()
        with torch.autocast(self.device, enabled=True):
            x1,m1,p1,x2,m2,p2 = self.waugment.forward(triplet)
            z1,y1,zp1 = self.target.forward(x1,p1)
            z2,y2,zp2 = self.target.forward(x2,p2)
            x = torch.cat([x1,x2], dim=0); y = torch.cat([y1,y2], dim=0); m = torch.cat([m1,m2], dim=0)
            loss: torch.Tensor = self.criterion.forward(zp1,zp2,y,x,m)
        return loss.item()

    def train_step(self, triplet):
        self.target.train()
        with torch.autocast(self.device, enabled=True):
            # here goes the augmentation to generate two views of the triplet (t, m, p)
            self.optimizer.zero_grad(set_to_none=True)
            x1,m1,p1,x2,m2,p2 = self.waugment.forward(triplet)
            z1,y1,zp1 = self.target.forward(x1,p1)
            z2,y2,zp2 = self.target.forward(x2,p2)
            x = torch.cat([x1,x2], dim=0); y = torch.cat([y1,y2], dim=0); m = torch.cat([m1,m2], dim=0)
            loss: torch.Tensor = self.criterion.forward(zp1,zp2,y,x,m)
        self.scaler.scale(loss).backward()
        self.scaler.step(self.optimizer)
        self.scaler.update()
        self.scheduler.step()
        return loss.item()