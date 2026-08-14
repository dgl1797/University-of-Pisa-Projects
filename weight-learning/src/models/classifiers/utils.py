import timm
import detectors

def get_model():
    model = timm.create_model("resnet18_cifar10", pretrained=True)
    return model

if __name__ == "__main__":
    model = get_model()
    print(model)  