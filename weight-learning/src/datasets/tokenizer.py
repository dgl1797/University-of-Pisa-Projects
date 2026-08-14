import torch, copy

class Tokenizer():
    def __init__(self, tokensize: int = 0, ignore_bn: bool = False, use_masks: bool = False):
        '''
            @Args:
                - tokensize: the size of a single token, default = 0 => tokensize is inferred from model's maximal neuron size
                - ignore_bn: flag indicating wether or not the batch_norm-like layers should be included in the tokenization
                - use_masks: flag indicating wether or not the padding masks should be returned with the tokens 
        '''
        self.tokensize = tokensize
        self.ignore_bn = ignore_bn
        self.use_masks = use_masks
    
    def tokenize_weights(self, checkpoint: dict[str, torch.Tensor]):
        # defining tokensize
        tokens = []; masks = []; tokensize = self.tokensize
        if tokensize == 0:
            for key in checkpoint.keys():
                if ("bn" in key or "downsample.1" in key or "batchnorm" in key) and self.ignore_bn: continue
                tmpsize = 0
                if "weight" in key: tmpsize = torch.prod(torch.tensor(checkpoint[key].shape[1:])) + (1 if key.replace("weight", "bias") in checkpoint else 0)
                elif "running_mean" in key or "running_var" in key: tmpsize = torch.prod(torch.tensor(checkpoint[key].shape[1:]))
                if tmpsize > tokensize: tokensize = tmpsize # tokensize = numero di pesi(+eventuale bias) sul neurone di dimensione massima
        tokensize = int(tokensize)

        # getting checkpoints' tokens
        idx = 0; pos = []
        for key in checkpoint.keys():
            if ("bn" in key or "downsample.1" in key or "batchnorm" in key) and self.ignore_bn: continue
            if "weight" in key or "running_mean" in key or "running_var" in key:
                w: torch.Tensor = checkpoint[key]; w = w.view(w.shape[0], -1) # n_neuroni x dimensione_neurone
                if "weight" in key and key.replace("weight", "bias") in checkpoint: 
                    b: torch.Tensor = checkpoint[key.replace("weight", "bias")]
                    w = torch.cat([w, b.unsqueeze(1)], dim=1) # concatena ad ogni neurone il suo bias
                
                ntokens = w.shape[1] // tokensize
                residual = w.shape[1] % tokensize
                token_factor = int(ntokens)
                if residual > 0: token_factor += 1

                # ogni neurone corrispondente allo stesso token ha la stessa coppia [idx,jdx] ma differisce per indice in idx_layer
                idx_layer = [[idx, jdx] for jdx in range(w.shape[0]) for _ in range(token_factor)]
                idx+=1; pos.extend(idx_layer) # l'indice in pos corrisponde all'indice del layer

                # aggiunge padding se un neurone è più piccolo di tokensize generando effettivamente un neurone paddato
                if residual > 0:
                    mask = torch.zeros(w.shape[0], tokensize * token_factor)
                    mask[:, :w.shape[1]] = torch.ones(w.shape)
                    wpad = torch.zeros(w.shape[0], tokensize * token_factor)
                    wpad[: , :w.shape[1]] = w
                    w = wpad # cambia reference puntata da w a wpad
                else: mask = torch.ones(w.shape[0], tokensize * token_factor) # la maschera indica che non c'è padding (tutti 1)

                # concatena i gruppi di tokensize che eccedono sulle colonne nelle righe di w e mask preservando l'ordine dei token
                w = w.view(-1, tokensize)
                mask = mask.view(-1, tokensize)
                tokens.append(w); masks.append(mask)
        
        # post-processing
        tokens = torch.cat(tokens, dim=0); masks = torch.cat(masks, dim=0)
        # esplicita l'index in pos, quindi il token_idx o neuron_idx (ndx) essendo che questo crese ad ogni coppia
        pos = torch.tensor([(ndx, idx, jdx) for ndx, (idx, jdx) in enumerate(pos)])
        return (tokens, masks, pos) if self.use_masks else (tokens, pos)
    
    def inject_tokens(self, checkpoint_reference: dict[str, torch.Tensor], tokens: torch.Tensor, pos: torch.Tensor):
        checkpoint = copy.deepcopy(checkpoint_reference)
        idx = 0
        for key in checkpoint.keys():
            if ("bn" in key or "downsample.1" in key or "batchnorm" in key) and self.ignore_bn: continue
            if "weight" in key or "running_mean" in key or "running_var" in key:
                ndx = torch.where(pos[:, 1] == idx)[0]
                w_t = tokens[ndx, :]
                contentlength = int(torch.prod(torch.tensor(checkpoint[key].shape[1:])))
                checkpoint[key] = w_t.view(checkpoint[key].shape[0], -1)[:, :contentlength].view(checkpoint[key].shape)
                if "running_var" in key: checkpoint[key] = checkpoint[key].clamp(min=0)
                if "weight" in key and key.replace("weight", "bias") in checkpoint: checkpoint[key.replace("weight", "bias")] = w_t.view(checkpoint[key].shape[0], -1)[:, contentlength]
                idx += 1
        return checkpoint
    
    def checkpoints_equals(self, ckpt1: dict[str, torch.Tensor], ckpt2: dict[str, torch.Tensor]):
        if not all([k1 == k2 for k1,k2 in zip(ckpt1.keys(), ckpt2.keys())]): return False
        for key in ckpt1.keys():
            if ("bn" in key or "downsample.1" in key or "batchnorm" in key) and self.ignore_bn: continue
            if "weight" in key or "running_mean" in key or "running_var" in key:
                if ckpt1[key].shape != ckpt2[key].shape: return False
                if torch.any((ckpt1[key] - ckpt2[key]).abs() > 0).item(): return False
            if "weight" in key and key.replace("weight", "bias") in ckpt1:
                newkey = key.replace("weight", "bias")
                if ckpt1[newkey].shape != ckpt2[newkey].shape: return False
                if torch.any((ckpt1[newkey] - ckpt2[newkey]).abs() > 0).item(): return False
        return True
    
    def randomize_checkpoint(self, target: dict[str, torch.Tensor], scale: float = 1e-3):
        randomized_checkpoint = copy.deepcopy(target)
        for key in randomized_checkpoint.keys():
            if ("bn" in key or "downsample.1" in key or "batchnorm" in key) and self.ignore_bn: continue
            if "weight" in key or "running_mean" in key or "running_var" in key or "bias" in key:
                noise = torch.rand_like(randomized_checkpoint[key])
                # noise_scale = randomized_checkpoint[key].min() * scale # o(min) to be negligible for the whole layer
                randomized_checkpoint[key] += noise * scale
                if "running_var" in key: randomized_checkpoint[key].clamp(min=0)
        return randomized_checkpoint

if __name__ == "__main__":
    from pathlib import Path
    ckpt = torch.load(Path("checkpoints", "resnet18_awa2.pt"))
    tokenizer = Tokenizer()

    tokens, positions = tokenizer.tokenize_weights(ckpt)
    reconstructed = tokenizer.inject_tokens(ckpt, tokens, positions)
    print(tokenizer.checkpoints_equals(reconstructed, ckpt))