import torch

class WindowPipeline(torch.nn.Module):
    def __init__(self, stack: list[torch.nn.Module]):
        super(WindowPipeline, self).__init__()
        self.stack = stack
    
    def forward(self, w: list[torch.Tensor]):
        out = w
        for s in self.stack: out = s.forward(*out)
        return out