import torch

class SimpleClone(torch.nn.Module):
    def __init__(self):
        super(SimpleClone, self).__init__()
    
    def forward(self, x: torch.Tensor, m: torch.Tensor, p: torch.Tensor):
        x2, m2, p2 = x.detach().clone().to(x.device), m.detach().clone().to(m.device), p.detach().clone().to(p.device)
        return x,m,p,x2,m2,p2