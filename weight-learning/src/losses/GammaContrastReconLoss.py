import torch

class NT_Xent(torch.nn.Module):
    def __init__(self, temperature: float, positive_only: bool = False):
        super(NT_Xent, self).__init__()
        self.temperature = temperature; self.positive_only = positive_only
        self.ce = torch.nn.CrossEntropyLoss(reduction="sum")
        self.similarity = torch.nn.CosineSimilarity(dim=2) # along embeddings dimension
    
    def mask_correlated_samples(self, batchsize: int):
        N = 2*batchsize
        mask: torch.Tensor = torch.ones((N, N), dtype=bool) # boolean indexing matrix for negative samples
        # 1 samples are the one considered as negative, so the one the model should diverge from
        mask = mask.fill_diagonal_(0)
        for i in range(batchsize):
            mask[i, batchsize + i] = 0 # diagonal batch_size (upper)
            mask[batchsize + i, i] = 0 # diagonal -batch_size (lower)
        return mask
    
    def forward(self, zi: torch.Tensor, zj: torch.Tensor):
        '''zi and zj are two embeddings of the same model in different views (after applying augmentation), shape: (Bs, Cn)'''
        batch_size = zi.shape[0]
        mask = self.mask_correlated_samples(batch_size)
        N = 2*batch_size
        z = torch.cat([zi, zj], dim=0) # along batch dimension

        # results in [Bs, Bs] where each element ij is the similarity of 
        # zi@i with zi@j(first batch-size elements) 
        # and zi@i with zj@j(second batch-size elements)
        sim: torch.Tensor = self.similarity(z.unsqueeze(1), z.unsqueeze(0)) / self.temperature
        sim_i_j = sim.diag(batch_size)
        sim_j_i = sim.diag(-batch_size)

        positive = torch.cat([sim_i_j, sim_j_i], dim=0).reshape(N, 1)
        negative = sim[mask].reshape(N, -1)

        labels = torch.zeros(N).to(positive.device).long()
        if self.positive_only: labels = labels.unsqueeze(dim=1)

        logits = torch.cat([positive, negative], dim=1) # positive samples always at position 0 to match labels
        loss = self.ce(positive, labels) if self.positive_only else self.ce(logits, labels)
        loss /= N
        return loss

class MaskedReconLoss(torch.nn.Module):
    def __init__(self, reduction: str):
        super(MaskedReconLoss, self).__init__()
        self.mse = torch.nn.MSELoss(reduction=reduction)
        self.loss_mean = None
    
    def forward(self, output: torch.Tensor, target: torch.Tensor, mask: torch.Tensor):
        assert (
            output.shape == target.shape == mask.shape
        ), f"MSE loss error: prediction and target don't have the same shape. output {output.shape} vs target {target.shape} vs mask {mask.shape}"

        loss = self.mse(mask*output, target)
        # rsq part with torchmetrics.functional.explained_variance non sembra utilizzato
        return loss
    
    # def set_mean_loss(self, data: torch.Tensor, mask: torch.Tensor):
    #     """
    #     #TODO - l'implementazione sembra incompleta
    #     """
    #     # check that data are tensor..
    #     assert isinstance(data, torch.Tensor)
    #     w_mean = data.mean(dim=0)  # compute over samples (dim0)
    #     # scale up to same size as data
    #     data_mean = repeat(w_mean, "l d -> n l d", n=data.shape[0])
    #     out_mean = self.forward(data_mean, data, mask)

    #     # compute mean
    #     print(f" mean loss: {out_mean['loss_recon']}")

    #     self.loss_mean = out_mean["loss_recon"]


class GammaContrastReconLoss(torch.nn.Module):
    def __init__(
        self, gamma: float, reduction: str, temperature: float, contrast: str = "simclr", 
        z_var_penalty: float = 0.0, z_norm_penalty: float = 0.0
    ):
        super(GammaContrastReconLoss, self).__init__()
        assert 0 <= gamma <= 1
        self.gamma = gamma; self.zvp = z_var_penalty; self.znp = z_norm_penalty
        self.contrast_loss = NT_Xent(temperature, positive_only = (contrast == "positive"))
        self.recon_loss = MaskedReconLoss(reduction)
    
    def forward(self, zi: torch.Tensor, zj: torch.Tensor, y: torch.Tensor, t: torch.Tensor, m: torch.Tensor):
        reconloss = self.recon_loss(y, t, m) # reconstruction loss using MSE between y reconstructed and target (original)
        contrloss = self.contrast_loss(zi, zj) # constrastive loss to guide convergence for positive samples and divergence for negative ones
        totalloss = self.gamma * contrloss + (1-self.gamma)*reconloss

        znorm = torch.linalg.norm(zi.view(zi.shape[0], -1), ord=2, dim=1).mean()
        zvar = torch.mean(torch.var(zi.view(zi.shape[0], -1), dim=0))

        totalloss = totalloss + self.zvp * zvar + self.znp * znorm
        return totalloss
    
    # def set_mean_loss(self, weights: torch.Tensor, mask=None) -> None:
    #     """
    #     Helper function to set mean loss in reconstruction loss - Sembra incompleta
    #     """
    #     # if mask not set, set it to all ones
    #     if mask is None:
    #         mask = torch.ones(weights.shape)
    #     # call mean_loss function
    #     self.loss_recon.set_mean_loss(weights, mask=mask)