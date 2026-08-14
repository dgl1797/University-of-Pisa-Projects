import torch, sys, os
from pathlib import Path

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")))
from src.datasets.tokenizer import Tokenizer
from src.implementations.classification import ClassificationTask
from src.models.classifiers.resnet18 import ResNet18

class TokenizedModelWeightDataset(torch.utils.data.Dataset):
    '''
    Tokenizes the passed checkpoint by using the received Tokenizer instance. Sets windowsize for the dataset and the stride.
    '''
    def __init__(
        self, checkpoint: dict[str, torch.Tensor], tokenizer: Tokenizer, windowsize: int = 1, 
        stride: int = None, fix_window: str = "shift"
    ):
        '''
        ### Arguments
            - checkpoint: the checkpoint to be tokenized
            - tokenizer: the Tokenizer instance to calculate tokens from checkpoints
            - windowsize: number of tokens to be considered in each sample
            - stride: as in convolutional layer, sets the sliding step of the window
            - fix_window: either "shift" or "padding". Allows to select a method for handling last incomplete window either
            by moving the last window starting point back, to match the windowsize (shift) or by zero-padding the remaining part
        '''
        assert windowsize > 0, f"{windowsize} invalid as window size, at least 1 token must be present in the window"
        tokens, masks, positions = tokenizer.tokenize_weights(checkpoint)
        self.tokens = tokens.detach().clone()
        self.masks = masks.detach().clone()
        self.positions = positions.detach().clone()
        assert self.tokens.shape[0] == self.masks.shape[0] and self.masks.shape[0] == self.positions.shape[0], f"Inconsistency in received checkpoint: tshape - {self.tokens.shape}, mshape - {self.masks.shape}, pshape - {self.positions.shape}"
        self.windowsize = min(windowsize, self.tokens.shape[0])
        self.stride = stride if stride is not None else self.windowsize
        self.fixwindow = fix_window

    def __len__(self):
        nwindows = (self.tokens.shape[0] - self.windowsize) // self.stride + 1
        
        # numero token in ultima finestra (per ora parametri settati per avere exceeding = 0)
        exceeding = (self.tokens.shape[0] - self.windowsize) % self.stride
        
        return nwindows + (1 if exceeding else 0)
    
    def tdim(self) -> int: return self.tokens.shape[1]
    
    def __getitem__(self, index):
        window_start = index*self.stride
        window_end = min(window_start+self.windowsize, self.tokens.shape[0])
        # shifting correction
        # if window_end-window_start < self.windowsize:
        #     if self.fixwindow == "shift": 
        #         window_start -= (self.windowsize + window_start - window_end)
        #         window_start = max(0, window_start)
        #         window_end = window_start + self.windowsize
        #         tk, mk, ps = (self.tokens[window_start:window_end, :], self.masks[window_start:window_end, :], self.positions[window_start:window_end, :])
        #         if self.transforms: tk = self.transforms(tk)
        #         return tk,mk,ps
        #     if self.fixwindow == "padding":
        #         tk, mk, ps = (self.tokens[window_start:window_end, :], self.masks[window_start:window_end, :], self.positions[window_start:window_end, :])
        #         padding_needed = self.windowsize - (window_end - window_start)
        #         last_util_index = ps[-1, 0].item(); fake_layer_index = ps[-1, 1]+1
                
        #         tkpad = torch.zeros(self.windowsize, tk.shape[1], dtype=tk.dtype)
        #         mkpad = torch.zeros(self.windowsize, mk.shape[1], dtype=mk.dtype)
        #         pspad = torch.tensor([[last_util_index+tdx+1, fake_layer_index, tdx] for tdx in range(padding_needed)], dtype=ps.dtype)

        #         tkpad[:tk.shape[0], :] = tk; mkpad[:mk.shape[0], :] = mk; pspad = torch.cat([ps, pspad], dim=0)
        #         if self.transforms: tkpad = self.transforms(tkpad)
        #         return tkpad,mkpad,pspad
        #     raise NotImplemented(f"available methods for window fixing: padding, shift, received: {self.fixwindow}")
        tk, mk, ps = (self.tokens[window_start:window_end, :], self.masks[window_start:window_end, :], self.positions[window_start:window_end, :])
        return tk,mk,ps

class AugmentedTokenizedWeightDataset(TokenizedModelWeightDataset):
    def __init__(
        self, checkpoint: dict[str, torch.nn.Module], nclasses: int, tokenizer: Tokenizer,
        windowsize: int = 1, nrandom: int = 1, stride: int = None, fix_window: str = "shift", device: str = "cpu",
        testloader: torch.utils.data.DataLoader = None, performance_loss_tolerance: float = 1e-1, step: float = 1e-1
    ):
        super(AugmentedTokenizedWeightDataset, self).__init__(checkpoint, tokenizer, windowsize, stride, fix_window)          
        self.props = []
        self.store_location = Path("data", "checkpoints", "noisy")
        self.store_location.mkdir(777, parents=True, exist_ok=True)
        generated_list = list(self.store_location.glob("*.pt"))
        if len(generated_list) > 0:
            self.tokens = []; self.masks = []; self.positions = []
            for g in generated_list:
                ckpt = torch.load(g, weights_only=False)
                tk,mk,ps = tokenizer.tokenize_weights(ckpt["state_dict"])
                self.tokens.append(tk); self.masks.append(mk), self.positions.append(ps)
                self.props.append(ckpt["metrics"])
            self.tokens = torch.cat(self.tokens, dim=0)
            self.masks = torch.cat(self.masks, dim=0)
            self.positions = torch.cat(self.positions, dim=0)
            return
        
        assert testloader is not None, "cannot build dataset because testloader is None"
        default_resnet = ResNet18(nclasses, False)
        discarding_task = ClassificationTask(None, nclasses, device)
        default_resnet.load_state_dict(checkpoint)
        original_metrics = discarding_task(default_resnet, testloader, "Generating Originial")
        self.props.append(original_metrics)
        torch.save({"state_dict": checkpoint, "metrics": original_metrics}, self.store_location.joinpath("origin.pt"))
        comparison_metric = original_metrics["accuracy"]
        ngenrated = 0
        scale = 9e-1 # 9e-5 consistently produces extremely close performances to original model
        while ngenrated < nrandom:
            randomized = tokenizer.randomize_checkpoint(checkpoint, scale)
            default_resnet.load_state_dict(randomized)
            randomized_metrics = discarding_task(default_resnet, testloader, f"Gen{ngenrated}")
            if comparison_metric - randomized_metrics["accuracy"] >= performance_loss_tolerance: 
                print(f"scale: {scale:.3e} Failed: {randomized_metrics['accuracy']:.4f} against {comparison_metric:.4f}")
                scale *= step
                continue
            print(f"scale: {scale:.3e} Success: {randomized_metrics['accuracy']:.4f} against {comparison_metric:.4f}")
            torch.save({"state_dict": randomized, "metrics": randomized_metrics}, self.store_location.joinpath(f"gen_{ngenrated}.pt"))
            ngenrated += 1
            tk, mk, ps = tokenizer.tokenize_weights(randomized)
            self.tokens = torch.cat([self.tokens, tk], dim=0)
            self.masks = torch.cat([self.masks, mk], dim=0)
            self.positions = torch.cat([self.positions, ps], dim=0)
            self.props.append(randomized_metrics)


if __name__ == '__main__':
    from src.datasets.images.generic_image import GenericImagesDataset
    from src.datasets.images.helpers import load_rows_from_csv, load_txt

    originckpt = torch.load(Path("checkpoints", "resnet18_awa2.pt"))
    nclasses = originckpt["model.fc.weight"].shape[0]
    tokenizer = Tokenizer(288, use_masks=True)
    csvloc = Path("data", "AWA2_Data", "image_splits", "base", "test.csv")
    fltloc = Path("data", "AWA2_Data", "class_splits", "tunseen.txt")
    testds = GenericImagesDataset(load_rows_from_csv(csvloc), load_txt(fltloc), transform="default")
    testloader = torch.utils.data.DataLoader(testds, 32, False, num_workers=4, persistent_workers=True)
    trainds = AugmentedTokenizedWeightDataset(originckpt, nclasses, tokenizer, 272, 5, 272//4, device="cuda", testloader=testloader)

    print(len(trainds), (trainds.tokens.shape[0] - trainds.windowsize) % trainds.stride)