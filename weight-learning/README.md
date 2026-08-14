# Install

To install the project, simply clone the repository and get the necessary dependencies:
```sh
git clone https://github.com/MarcoParola/weight-learning.git
cd weight-learning
```

Create the virtualenv (you can also use conda) and install the dependencies of *requirements.txt*

```bash
python -m venv env
. env/bin/activate
python -m pip install -r requirements.txt
```

Next, create a new project on [Weights & Biases](https://wandb.ai/site) named `weight-learning`. Edit `entity` parameter in [wandb  configuration file](./config/loggers/wandb.yaml) by setting your wandb nick. Log in and paste your API key when prompted.
```sh
wandb login 
```

# Runs

## Sane Training

To run the adapted version of [SANE](https://github.com/HSG-AIML/SANE) training, download a trained version of a [resnet18-torchvision architecture](https://docs.pytorch.org/vision/main/models/generated/torchvision.models.resnet18.html#torchvision.models.resnet18), taking care that all the layers names are fully matched as the models merging-matching has not yet been developed. Download the relative image dataset on which it has been trained/finetuned on, and configure paths in the [sane configuration file](./config/sane.yaml#L19). In this same file all architectural and training configurations can also be found, those parameters can be overridden both by code and by command-line as shown in [the framework's example code](./src/utils/config_manager.py#L77).

Once the configuration is set, you can run the sane_training experiment by executing the following command:
```sh
python ./experiment/sane_training.py -n [run_name] -t [type(either 'base' or 'augment')]
```

The base version will just try to overfit the training-checkpoint while the augment version will generate nrandom noisy versions and use those as well as the training checkpoint to try enhancing model's performances by exploring the near-surroundings of the model in the weight space. Those noisy versions will be saved in ./data/checkpoints and stay the same for all multiruns

# Extending the experiments

The framework uses an omegaconf-based configuration decorator that allows for: 
1. parameters merging using the keyword "import" inside the .yaml file, which allows to merge all the configurations for imported files into the current .yaml configuration
2. parameters override by code using dot-separated keys ("training.batchsize": 47) and by command line by passing the -o\[--override\] parameter which accepts a list of key=value elements (-o training.batchsize=47 training.nepochs=150)
3. multirun execution with different parameters selection using the same method of overrides but with list of values instead of single values as argument ("training.batchsize": \[25, 47\]), and also a cli -m which accepts key=value1,value2 arguments (-m training.batchsize=\[25,47\] training.nepochs=150,1000 _both notations are accepted_)

To add a new experiment, implement it in the [experiment folder](./experiments/) placing all necessary datasets/models and whatever else in the [source directory](./src/), add a [schema](./src/utils/schemas/) for the configuration of the experiment, and a [.yaml configuration file](./config/).

Finally wrap the experiment with the ```@use_config``` decorator specifying the configuration file location relatively to the [config directory](./config/), the earlier created schema, and all the desired overrides/multiruns.