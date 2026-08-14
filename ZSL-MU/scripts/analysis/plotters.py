from numpy import ndarray, arange as npyarange, abs as npyabs
from numpy.random import choice as npychoice
import matplotlib.pyplot as plt

def plot_heatmaps(original_os: ndarray, golden_os: ndarray, original_fs: ndarray, golden_fs: ndarray, translated_cids: list[int], cids: list[int]):
    fig, axes = plt.subplots(3, 2, figsize=(9,7))

    oosa = axes[0][0].imshow(original_os[translated_cids, :] if translated_cids is not None else original_os, cmap='coolwarm', origin="upper", aspect="auto")
    axes[0][0].set_title("origin one-shot")
    axes[0][0].set_xlabel("neurons"); axes[0][0].set_ylabel("classes")
    if translated_cids is not None: axes[0][0].set_yticks(npyarange(len(cids))); axes[0][0].set_yticklabels(cids)
    oosa_bar = fig.colorbar(oosa, ax=axes[0][0], orientation="vertical", fraction=0.046, pad=0.04)
    oosa_bar.set_label('Activation Levels')

    ofsa = axes[0][1].imshow(original_fs[translated_cids, :] if translated_cids is not None else original_fs, cmap="coolwarm", origin="upper", aspect="auto")
    axes[0][1].set_title("origin full-shot-average")
    axes[0][1].set_xlabel("neurons"); axes[0][1].set_ylabel("classes")
    if translated_cids is not None: axes[0][1].set_yticks(npyarange(len(cids))); axes[0][1].set_yticklabels(cids)
    ofsa_bar = fig.colorbar(ofsa, ax=axes[0][1], orientation="vertical", fraction=0.046, pad=0.04)
    ofsa_bar.set_label('Activation Levels')

    gosa = axes[1][0].imshow(golden_os[translated_cids, :] if translated_cids is not None else golden_os, cmap='coolwarm', origin="upper", aspect="auto")
    axes[1][0].set_title("golden one-shot")
    axes[1][0].set_xlabel("neurons"); axes[1][0].set_ylabel("classes")
    if translated_cids is not None: axes[1][0].set_yticks(npyarange(len(cids))); axes[1][0].set_yticklabels(cids)
    gosa_bar = fig.colorbar(gosa, ax=axes[1][0], orientation="vertical", fraction=0.046, pad=0.04)
    gosa_bar.set_label('Activation Levels')

    gfsa = axes[1][1].imshow(golden_fs[translated_cids, :] if translated_cids is not None else golden_fs, cmap="coolwarm", origin="upper", aspect="auto")
    axes[1][1].set_title("golden full-shot-average")
    axes[1][1].set_xlabel("neurons"); axes[1][1].set_ylabel("classes")
    if translated_cids is not None: axes[1][1].set_yticks(npyarange(len(cids))); axes[1][1].set_yticklabels(cids)
    gfsa_bar = fig.colorbar(gfsa, ax=axes[1][1], orientation="vertical", fraction=0.046, pad=0.04)
    gfsa_bar.set_label('Activation Levels')

    # differences:
    diff_map = npyabs(golden_os[translated_cids, :] - original_os[translated_cids, :]) if translated_cids is not None else npyabs(golden_os - original_os)
    diff_osa = axes[2][0].imshow(diff_map, cmap="coolwarm", origin="upper", aspect="auto")
    axes[2][0].set_title("difference one-shot")
    axes[2][0].set_xlabel("neurons"); axes[2][0].set_ylabel("classes")
    if translated_cids is not None: axes[2][0].set_yticks(npyarange(len(cids))); axes[2][0].set_yticklabels(cids)
    dosa_bar = fig.colorbar(diff_osa, ax=axes[2][0], orientation="vertical", fraction=0.046, pad=0.04)
    dosa_bar.set_label('Activation Levels')

    diff_map = npyabs(golden_fs[translated_cids, :] - original_fs[translated_cids, :]) if translated_cids is not None else npyabs(golden_fs - original_fs)
    diff_fsa = axes[2][1].imshow(diff_map, cmap="coolwarm", origin="upper", aspect="auto")
    axes[2][1].set_title("difference full-shot-averaged")
    axes[2][1].set_xlabel("neurons"); axes[2][1].set_ylabel("classes")
    if translated_cids is not None: axes[2][1].set_yticks(npyarange(len(cids))); axes[2][1].set_yticklabels(cids)
    dfsa_bar = fig.colorbar(diff_fsa, ax=axes[2][1], orientation="vertical", fraction=0.046, pad=0.04)
    dfsa_bar.set_label('Activation Levels')

    plt.tight_layout()
    plt.show()
    return None

def plot_boxplots(original_os: ndarray, golden_os: ndarray, original_fs: ndarray, golden_fs: ndarray, translated_cids: list[int], cids: list[int]):
    all_datasets = {
        'Origin One-Shot': original_os[translated_cids, :] if translated_cids is not None else original_os,
        'Golden One-Shot': golden_os[translated_cids, :] if translated_cids is not None else golden_os,
        'Origin Full-Shot Avg': original_fs[translated_cids, :] if translated_cids is not None else original_fs,
        'Golden Full-Shot Avg': golden_fs[translated_cids, :] if translated_cids is not None else golden_fs,
    }

    num_datasets = len(all_datasets)
    num_classes = list(all_datasets.values())[0].shape[0]

    plt.figure(figsize=(18, 2 * num_datasets))
    class_labels = [i for i in cids] if translated_cids is not None else [i for i in range(num_classes)]

    for i, (dataset_name, data_matrix) in enumerate(all_datasets.items()):
        ax = plt.subplot(num_datasets, 1, i + 1)

        plt.boxplot(data_matrix.T) # requires classes in columns dimension
        plt.title(dataset_name)
        if translated_cids is not None: plt.xlabel('Classes')
        plt.ylabel('Activation Level')

        if translated_cids is not None: plt.xticks(npyarange(1, num_classes + 1), class_labels)
        else: plt.xticks([])
        plt.grid(axis='y', linestyle='--', alpha=0.7)

    plt.tight_layout()
    plt.show()
    return None