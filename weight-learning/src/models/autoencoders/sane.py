"""
    References:
        Konstantin Schürholt's Scalable Autoencoder for Neural Embeddings: https://github.com/HSG-AIML/SANE
"""

import torch, math, sys, os

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")))
from src.models.transformers.gpt2 import GPTransformer

class PositionalEmbs(torch.nn.Module):
    def __init__(self, max_positions: list[int], embedding_dimension: int):
        super(PositionalEmbs, self).__init__()
        self.max_positions = max_positions; self.embedding_dimension = embedding_dimension
        if len(max_positions) == 2:
            self.pe1 = torch.nn.Embedding(max_positions[0], embedding_dimension // 2)
            self.pe2 = torch.nn.Embedding(max_positions[1], embedding_dimension // 2)
            self.pe3 = None
        elif len(max_positions) == 3:
            self.pe1 = torch.nn.Embedding(max_positions[0], embedding_dimension // 2)  # add 1 + 2
            self.pe2 = torch.nn.Embedding(max_positions[1], embedding_dimension // 2)  # add 1 + 2
            self.pe3 = torch.nn.Embedding(max_positions[2], embedding_dimension // 2)  # cat 1+2 & 3
    
    def forward(self, inputs: torch.Tensor, positions: torch.Tensor):
        assert inputs.ndim == 3, f"Expecting 3D-tensor as input but got f{inputs.ndim}"
        assert positions.shape[2] == len(self.max_positions), f"Positions should have {len(self.max_positions)} dimensions, got: {positions.shape[2]}"
        assert positions.shape[0] == inputs.shape[0] and positions.shape[1] == inputs.shape[1], f"positions and inputs should have same shapes along dimensions 0 and 1"

        pe1 = self.pe1(positions[:, :, 0])
        pe2 = self.pe2(positions[:, :, 1])
        posemb = [pe1, pe2]
        if self.pe3 is not None: pe3 = self.pe3(positions[:,:,2]); posemb = [pe1+pe2, pe3]
        posemb = torch.cat(posemb, dim=2)
        return inputs + posemb

class ProjectionHead(torch.nn.Module):
    def __init__(self, latdim: int, ntokens: int, odim: int):
        super(ProjectionHead, self).__init__()
        self.head = torch.nn.Sequential(
            torch.nn.Linear(latdim*ntokens, odim, bias=False),
            torch.nn.LayerNorm(odim),
            torch.nn.ReLU(),
            torch.nn.Linear(odim, odim, bias=False),
            torch.nn.LayerNorm(odim),
            torch.nn.ReLU(),
        )
    
    def forward(self, z: torch.Tensor):
        z = z.view(z.shape[0], -1) # (Bs, Tn, Ed) into (Bs, Tn*Ed)
        return self.head(z)

class SANE(torch.nn.Module):
    # default values taken from: https://github.com/HSG-AIML/SANE/blob/main/experiments/resnet18-cifar100/pretrain_sane_cifar100_resnet18.py
    def __init__(self, 
        idim: int = 288, edim: int = 2048, nhead: int = 16, nblocks: int = 8, latdim: int = 128, wsize: int = 256, 
        dropout: float = 0.0, max_positions: list[int] = [55000, 100, 550]
    ):
        super(SANE, self).__init__()
        assert edim % nhead == 0, f"Transformers need to divide embeddings in multiple attention heads, make sure edim and nhead are multiples"
        self.tokenizer = torch.nn.Linear(idim, edim)
        self.transformer_encoder = GPTransformer(nblocks, wsize, nhead, edim, dropout, bias=False, causal=False)
        self.encoder_comp = torch.nn.Linear(edim, latdim)
        self.decoder_comp = torch.nn.Linear(latdim, edim)
        self.transfomer_decoder = GPTransformer(nblocks, wsize, nhead, edim, dropout, bias=False, causal=False)
        self.detokenizer = torch.nn.Linear(edim, idim)
        self.pe = PositionalEmbs(max_positions, edim)
        self.projection_head = ProjectionHead(latdim, wsize, odim=30)
        self.dropout = torch.nn.Dropout(dropout)

        # taken from Kaparthy's GPT2 implementation:
        self.apply(self._init_weights)
        # apply special scaled init to the residual projections, per GPT-2 paper
        for pn, p in self.named_parameters():
            if pn.endswith("projection.weight"):
                torch.nn.init.normal_(p, mean=0.0, std=0.02 / math.sqrt(2 * nblocks))

    def _init_weights(self, module):
        if isinstance(module, torch.nn.Linear):
            torch.nn.init.normal_(module.weight, mean=0.0, std=0.02)
            if module.bias is not None:
                torch.nn.init.zeros_(module.bias)
        elif isinstance(module, torch.nn.Embedding):
            torch.nn.init.normal_(module.weight, mean=0.0, std=0.02)
    
    def encode(self, x: torch.Tensor, p: torch.Tensor, m: torch.Tensor = None):
        # get a token from x
        x = self.tokenizer(x)
        # add positional encoding
        x = self.pe(x, p)
        x = self.dropout(x)
        x = self.transformer_encoder(x,m)
        x = self.encoder_comp(x)
        # return compressed encoding of token+posemb
        return x
    
    def decode(self, z: torch.Tensor, p: torch.Tensor, m: torch.Tensor = None):
        # decode compressed encoding of token+posemb
        z = self.decoder_comp(z)
        # add positional encoding
        z = self.pe(z, p)
        z = self.dropout(z)
        z = self.transfomer_decoder(z,m)
        z = self.detokenizer(z)
        # return detokenized decoded weights
        return z

    def forward(self, x: torch.Tensor, p: torch.Tensor, m: torch.Tensor = None):
        z = self.encode(x, p, m)
        zp = self.projection_head(z)
        y = self.decode(z, p, m)
        return z,y,zp
    
    def forward_embeddings(self, x: torch.Tensor, p: torch.Tensor) -> torch.Tensor:
        x = self.encode(x,p)
        return x.mean(dim=1)

if __name__ == '__main__':
    sane = SANE().to("cuda")
    print(sane)
    print(f"Allocated CUDA Memory: {torch.cuda.max_memory_allocated() / (1024**3):.4f} GB")