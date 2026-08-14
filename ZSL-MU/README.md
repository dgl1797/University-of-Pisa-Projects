# ZSL-MU

[![license](https://img.shields.io/static/v1?label=OS&message=Linux&color=green&style=plastic)]()
[![Python](https://img.shields.io/static/v1?label=Python&message=3.9&color=blue&style=plastic)]()


The goal of this project is to investigate the challenges of Machine Unlearning and Zero-Shot Learning in the context of classification, and to develop a novel approach that combines both techniques in a completely image-free setting. The proposed method leverages class annotations, along with the model’s existing weights and biases, to generate new parameters that can be injected into the model in a zero-shot-like manner. This approach aims to enhance the model’s flexibility in dynamic environments where data privacy is a critical concern.

It is built on top of the Hydra and Optuna frameworks to facilitate experiment sweeping and hyperparameter optimization. An additional goal is to maintain a modular and customizable structure, enabling easy integration of new features and supporting multi-run executions. *For further informations about the framework, check the [usage guide](./docs/usage/readme.md).*

Each result is documented in its corresponding section below:
- [Origins](./docs/results/readme.md)
- [Unlearn Baselines](./docs/results/mu/readme.md)
- [ZSLearn Baselines](./docs/results/zsl/readme.md)