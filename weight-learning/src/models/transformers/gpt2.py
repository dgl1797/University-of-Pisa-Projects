"""
    References:
    Andrey Kaparthy's NanoGpt: https://github.com/karpathy/nanoGPT/blob/master/model.py
        - the official GPT-2 TensorFlow implementation released by OpenAI: https://github.com/openai/gpt-2/blob/master/src/model.py
        - huggingface/transformers PyTorch implementation: https://github.com/huggingface/transformers/blob/main/src/transformers/models/gpt2/modeling_gpt2.py
"""

import torch

class LayerNorm(torch.nn.Module):
    def __init__(self, ndim: int, bias: bool):
        super(LayerNorm, self).__init__()
        self.weight = torch.nn.Parameter(torch.ones(ndim))
        self.bias = torch.nn.Parameter(torch.zeros(ndim)) if bias else None
    
    def forward(self, x: torch.Tensor):
        return torch.nn.functional.layer_norm(x, self.weight.shape, self.weight, self.bias, 1e-5)

class MLP(torch.nn.Module):
    def __init__(self, edim: int, dropout: float, bias: bool):
        super(MLP, self).__init__()
        self.expansion = torch.nn.Linear(edim, 4*edim, bias)
        self.activation = torch.nn.GELU()
        self.projection = torch.nn.Linear(edim*4, edim, bias)
        self.dropout = torch.nn.Dropout(dropout)
    
    def forward(self, x: torch.Tensor): return self.dropout(self.projection(self.activation(self.expansion(x))))

class SelfAttention(torch.nn.Module):
    def __init__(self, edim: int, nhead: int, dropout: bool, bias: bool, causal: bool, blocksize: int):
        super(SelfAttention, self).__init__()
        # instead of instantiating one Linear for each of key,query,value, just instantiate 1 Linear with 3*input_dim encoding all
        self.qkv = torch.nn.Linear(edim, 3*edim, bias)
        self.projection = torch.nn.Linear(edim, edim, bias)

        self.attn_dropout = torch.nn.Dropout(dropout); self.resid_dropout = torch.nn.Dropout(dropout)
        self.nhead = nhead
        self.edim = edim
        self.dropout = dropout
        self.causal = causal
        self.blocksize = blocksize
        
        '''
            causal masking: torch.tril(torch.ones(blocksize, blocksize)).view(1,1,blocksize,blocksize):
                Triangular Left-Lower matrix from diagonal=0 to take only past elements (causality of current embedding)
                so that the attention is applied only to past embeddings and not also future ones 
        '''
    
    def forward(self, x: torch.Tensor, mask: torch.Tensor = None):
        Bs,Tn,Ed = x.shape # batch_size, token_number, embedding_dimension
        q,k,v = self.qkv(x).split(self.edim, 2) # (bs,tn,ed) b@ (edim, 3*edim) -> (bs, tn, 3*edim) -> split(split_size=edim, dim=2) -> 3x(bs,tn,edim)

        # for next matmul nhead needs to be the batchsize
        q = q.view(Bs, Tn, self.nhead, Ed // self.nhead).transpose(1,2)
        k = k.view(Bs, Tn, self.nhead, Ed // self.nhead).transpose(1,2)
        v = v.view(Bs, Tn, self.nhead, Ed // self.nhead).transpose(1,2)

        # layer-wise self attention doesn't need to be causal as layer ordering doesn't care of causality
        y = torch.nn.functional.scaled_dot_product_attention(q,k,v,mask, dropout_p=self.dropout if self.training else 0, is_causal=self.causal)

        '''             scaled_dot_product_attention:
            att = (q @ k.transpose(-2, -1)) * (1.0 / math.sqrt(k.size(-1))) # k.transpose(Tn,Ed) => k = (Ed,Tn) so that Ed is matmul with Ed
            if self.causal:
                att = att.masked_fill(
                    # places -inf where self.bias[:,:,Tn,Tn] is 0 so that softmax results in 0 relevance for future tokens (causality)
                    self.bias[:, :, :Tn, :Tn] == 0, float("-inf")
                )  # causal attention: masking future tokens, upper right triangular matrix if causality is required
            att = F.softmax(att, dim=-1) # gets relevance of causal attention values
            att = self.attn_dropout(att)
            y = att @ v  # (Bs, Nh, Tn, Tn) x (Bs, Nh, Tn, Hs) -> (Bs, Nh, Tn, Hs) # sums attention values with corresponding v learned embd
        '''

        y = y.transpose(1,2).contiguous().view(Bs,Tn,Ed) # restores y view and memory arrangement
        return self.resid_dropout(self.projection(y))

class Block(torch.nn.Module):
    def __init__(self, edim: int, nhead: int, dropout: float, bias: bool, causal: bool, blocksize: int):
        super(Block, self).__init__()
        self.ln_1 = LayerNorm(edim, bias)
        self.attn = SelfAttention(edim, nhead, dropout, bias, causal, blocksize)
        self.ln_2 = LayerNorm(edim, bias)
        self.mlp = MLP(edim, dropout, bias)
    
    def forward(self, x: torch.Tensor, mask: torch.Tensor = None):
        x = x + self.attn(self.ln_1(x), mask)
        x = x + self.mlp(self.ln_2(x))
        return x

class GPTransformer(torch.nn.Module):
    # default values taken from https://github.com/HSG-AIML/SANE/blob/main/experiments/resnet18-cifar100/pretrain_sane_cifar100_resnet18.py
    def __init__(self, nblocks: int = 8, blocksize: int = 256, nhead: int = 16, edim: int = 2048, dropout: float = 0.0, bias: bool = False, causal: bool = False):
        super(GPTransformer, self).__init__()
        self.nblocks = nblocks
        self.transformer = torch.nn.ModuleList([Block(edim, nhead, dropout, bias, causal, blocksize) for _ in range(nblocks)])
    
    def forward(self, x: torch.Tensor, mask: torch.Tensor = None):
        for block in self.transformer: x = block(x, mask)
        return x