# **Installation**

To install the project, simply clone the repository and get the necessary dependencies. Then, create a new project on [Weights & Biases](https://wandb.ai/site). Log in and paste your API key when prompted.
```sh
# clone repo
git clone https://github.com/MarcoParola/ZSL-MU.git
cd ZSL-MU
mkdir models data

# Create virtual environment and install dependencies 
python -m venv env
. env/bin/activate
python -m pip install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cu126
python -m pip install -r env/requirements.txt 

# Weights&Biases login 
wandb login 
```

Edit your wand user in the [respective runlog file](../../config/experiment/runlog/wandb.yaml#L2)

# Framework

The framework is built upon [hydra](https://hydra.cc/docs/intro/) to easen the development of experiments by automatically managing the dispatching of the correct task using the [configuration file system](../../config) and the creation of experiment dispatchers.

## [Scripts](../../scripts)

To make scripts independent from hydra-based experiments, all script files use argparse as argument parser and allow for multiple operations that can be useful during experiments development. Each of the scripts uses argparse library's builtin help.

- manage_experiment.py --> creates/deletes the [dispatchers](../../experiments/), the [data types](../../src/utils/ConfigTypes.py), the [.yaml configuration file](../../config/experiment/) for the experiment, and the [implementation of the experiment method](../../src/implementations/). As arguments it is possible to pass a name and a group + the delete optional argument that sets the scripts in deletion mode. For more info check the help using 
```sh
python ./scripts/manage_experiment.py --help
```
- print_models_info.py --> allows to display models informations, like available pytorch versions, weights and archtecture

- tests.py --> module used to test features separately from hydra-based experiments, as for instance to test a dataset loader without executing the entire experiment

- upload_files_to_wandb.py --> as this framework allows for wandb data logging, it is possible to load the entire output generated folder containing experiments using this script.

## [Data Preparation Scripts](../../scripts/data_preparation/)

With these scripts it is possible to manage datasets by executing operations like download, extraction and splits creation, keeping the data preprocessing separate from the experiments. The actual setup includes a [prepare_data.py](../../scripts/data_preparation/prepare_data.py) script used to customize the download and extraction process of each dataset using a default data structure, keeping the data root customizable:

- [./data/archives](../../data/archives/) contains all the downloaded archives for each dataset, it is created and managed by the [download function](../../scripts/data_preparation/prepare_data.py#L5) within data preparation script, and uses the information inside [download_info configuration folder](../../config/download_info/).

- for each dataset, a *dsname.upper()_Data* folder is created in the data_root folder by using the [relative extractor function](../../scripts/data_preparation/extractors.py) also responsible of creating the classnames-to-classids *classes.csv* mappings, the classids-to-pathlocation *images.csv* and the attribids-to-attribname mappings in *attribs.csv*. It also extracts the unseen splits as those are already standardized by the litterature of ZSL problem.

There are also split-generator scripts that keep the image-relative splits (train; test; validation) separated from the label-relative splits (seen; unseen; forget; retain):

- [create_image_split.py](../../scripts/data_preparation/create_image_split.py) --> for each passed key-value pair, creates a file in ./*data_root*/*DSNAME*_Data/image_splits/*key*.csv having *value*% of the total number of images for each class. The image split is also used as the base of a run name so that it is possible to start a completely new branch of experiments by either creating a new image split or by cloning an existing one with a different name.

- [create_forget_set.py](../../scripts/data_preparation/create_forget_set.py) --> generates a *splitname*.txt file containing the forget set for a given dataset locating it into ./*data_root*/*DSNAME*_Data/class_splits/*splitname*.txt. It is possible to customize how to extract those classes. It is highly reccomended to use a subset of a validation-unseen split as those are studied and standardized to exclude overlapping labels with ImageNet1k in order to avoid forget labels to be in the pre-training of a classifier.

- [make_attributes.py](../../scripts/data_preparation/make_attributes.py) --> uses a class named [AttributeMaker](../../scripts/data_preparation/make_attributes.py#L3) to allow customizable attribute generation using various methods. Currently only binary attributes or extracted from dataset file are possible, but can be extended by adding a method inside the AttributeMaker class and adding it in the [valid options for argparse](../../scripts/data_preparation/make_attributes.py#L64). It will be automatically called exploiting the getattr function of python.

## Runs Dispatching

Each experiment is ran on hydra, attatching the method configuration file over the [default one](../../config/conf.yaml). There are two kind of experiments: isolated ones like [origin](../../experiments/origin.py), and methods belonging to either unlearning or zero-shot learning categories. Isolated ones directly call the [respective implementation file](../../src/implementations/origin.py) while the methods are called from within the respective implementation subfolder. The structure of the implementation and the dispatcher stays the same:
- train function --> calls the prepare_train function that returns a state containing all the necessary to run the training of the experiment's model, then calls the train function in the same file returning the resulting trained model.
- hfdownload function --> downloads weights of trained version of the model directly from huggingface and stores it in the run's output
- load function --> loads an existing checkpoint for that run and returns it
- evaluate function --> similarly to train, calls the prepair_evaluate function that retuns a state containing all the necessary to run the evaluation, then calls the evaluate function of the same implementation file
- hpo function --> used to get the hyperparameters optimization function for the method, it prepares the *study*.db location and branch and then calls the hpo function within the implementation file to execute the study
- test function --> mainly used to test experiment-specific implementations or functionalities before including them in the implementation file

Each run is then stored within the output directory, it is possible to execute the different functions above by overriding the [task parameter](../../config/conf.yaml#L7) either manually in the file, or by command line:
```sh
python ./main.py task=evaluate experiment=unlearn experiment/method=badt experiment/dataset=awa2
```
The output directory generated by each experiment is divided into checkpoints(stored after training process); evaluations(stored after evaluation process); hpos(stored after hpo study). Inside of each a directory containing a file for each branch is created:
- **out/checkpoints/\*run/branch.pt** will be the generated checkpoint
- **out/evaluations/\*run/branch.npy** will be the generated evaluation file
- **out/hpos/\*run/study.db** will be the generated study, explorable using Optuna Dashboard where each branch study is stored separately.

run path and branch name are compiled inside the [main.py file](../../main.py#L28) which also handles output files creation and management.

## [SRC](../../src/)

As mentioned each experiment has its own implementation file, located directly within ./src/implementations directory or in the subfolder relative to the experiment, depending on its typology. The src folder, furthermore, also includes implementation of side utility functions and addons that are needed more than once or that are meant to be separate from single experiments' implementation.

ZSLearn and Unlearn experiments require a stored Origin checkpoint for the same model, make sure to run the relative origin experiment
with task set to "hfdownload" before running an unlearning or zslearning experiment. It is possible, for instance, to download all the resnet18 origins' weights from huggingface using the following command
```sh
python ./main.py -m task=hfdownload experiment=origin experiment/classifier=resnet18 experiment/dataset=cub,awa2,apy
```