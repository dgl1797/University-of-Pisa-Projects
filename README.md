# Thesis
Code and [Paper](https://github.com/dgl1797/Thesis/blob/main/Thesis_Paper.pdf) of the Thesis, developed during academic studies at University of Pisa

## Goals
Implement a unified approach able to adapt in zero-shots Computer Vision networks to new downstream tasks using as example:
* Unseen Labels Learning - Zero Shot Learning
* Seen Labels Unlearning - Machine Unlearning

and that acts directly on trained CNN's weights producing an embedding from a sequence of weights and generating a modified version using a form of input influence token.

## Methodology
To effectively implement the embedding system, it has been used an autoencoder method that learns a latent space from a subset of the CNN weights to then return to the original space applying a class description driven modification:
* Learning - a dataset is constructed with pairs $(w, l)$ where w are is the flattened subset of weights and l the label. The label is then augmented with a semantic brief description retrieved by Wikipedia and transformed via BERT's SentenceTransformer into an embedding that is also encoded in the same latent space using cross-autoencoding. For the reconstruction the semantic description aswell as the weights are reconstructed using semantic similarity and L2 norm for, respectively, label and weights reconstructions
* Unlearning - the same dataset is used but the convergence is applied to a noised version of the ground truth to induce confusion in the final learned weights

The approach has been then generalized to all the meaningful trainable weights of a CNN (excluding normalization and averaging layers) by utilizing a Transformer-based autoencoder inspired to [SANE](https://arxiv.org/html/2406.09997v1) with additional benefits like an increased explainability of the hyper-representation space learned by the model that is able to clusterize different tasks weights and spot specific embeddings responsible of solving specific tasks

## Code
ZSL-MU - contains the baselines and a first implementation of an autoencoder able to solve both tasks separately
weight-learning - contains the SANE's adaptation to the weight learning unified approach to learn the hyper-representation space
