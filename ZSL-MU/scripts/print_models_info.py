import argparse

import torchvision.models as models

def list_torchvision_models(has: str = None):
    models_list = models.list_models(models)
    if has:
        models_list = [m for m in models_list if has in m]
    print(models_list)

def list_torchvision_weights(model_name, has: str = None):
    model_weights = [mw for mw in models.get_model_weights(model_name)]
    if has:
        model_weights = [mw for mw in model_weights if has in mw]
    print(model_weights)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument("-mn", "--model-name", type=str, default="resnet18", help="allows to specify which model to print")
    excl_group = parser.add_mutually_exclusive_group()
    excl_group.add_argument("-ml", "--model-list", action='store_true', default=False, help="lists available models")
    excl_group.add_argument("-wl", "--weights-list", action='store_true', default=False, help="lists available weights for a model, default is resnet18")
    excl_group.add_argument("-p", "--print", action='store_true', default=False, help="prints a model architecture, default is resnet18's arch")
    arguments = vars(parser.parse_args())

    if "weights_list" in arguments and arguments["weights_list"]:
        list_torchvision_weights(arguments["model_name"])
    elif "print" in arguments and arguments["print"]:
        print(models.get_model(arguments["model_name"]))
    else:
        list_torchvision_models(has=arguments["model_name"])