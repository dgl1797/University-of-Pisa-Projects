# Zero-Shot Learning Baselines

Here is presented the zero-shot learning problem for classification, together with a couple of methods that could be adapted to Image-free Zero-Shot Learning.

*In classification problems, zero-shot learning is any method that allows a model $\theta$ trained on a set of seen labels $S$, to be able to transfer its learning to a set of unseen labels $U$ (ZSL) or even to extend its knowledge to that unseen set, hence being able to predict any label $y \in S \cup U$ (Generalized-ZSL).*

For Image-free Zero-shot Learning we consider a scenario where the images used to train the original model, aren't available anymore, thus the method we are using as baselines will be adapted to this assumption. 
As unseen set, the labels that were filtered out from origin training are used.

### [ConSE](../../../src/implementations/zslearn/conse.py)

Convex-combination of Semantic Embeddings uses a mathematical approach to transfer the model's predictions to the new set of labels by computing a convex combination of the seen labels and the respective semantic embedding, and then comparing it to the unseen set of labels using cosine similarity ranking to get the most-likely unseen label.

$$conse = \sum_{y_i \in S}{p(y_i | x_i) * s(y_i)}$$

It can also be used with the top T predictions only by adapting the formula for the conse_embedding to:

$$conse = \frac{1}{\sum_{t=1}^{T}{p(y_i, t | x_i)}} * \sum_{t=1}^{T}{p(y_i, t | x_i) * s(y_i)}$$

where $p(y_i, t | x_i)$ is the model's top-t predicted label, for the sample $x_i$

this embedding is thus used to measure the cosine similarity with each $y_u \in U$ in ZSL, while for G-ZSL it is measured with each $y \in S \cup U$.

As semantic space, the original labels' annotations are used instead of the normalized ones as those had slightly better performances, which are extracted by using the [make_attributes.py script](../../../scripts/data_preparation/make_attributes.py) in the following configuration:
```sh
python ./scripts/data_preparation/make_attributes.py -d cub awa2 apy -m extract
```

Those are the results obtained by running the command below with the specified [method's configuration file](../../../config/experiment/method/conse.yaml)
```sh
python ./main.py -m experiment=zslearn experiment/method=conse experiment.method.embeddings=original_attribs experiment/dataset=cub,awa2,apy
```

<div class="metrics-table">

**Dataset**|**ZSL Accuracy**|**GZSL Accuracy**|**GZSL Accuracy on $U$**|**GZSL Accuracy on $S$**|**Harmonic Accuracy**|**Run-Time**
:---:|:---:|:---:|:---:|:---:|:---:|:---:
CUB|0.30259|0.53086|0.01729|0.70388|0.03375|29s
AWA2|0.39313|0.72054|0.00172|0.91947|0.00343|48s
APY|0.29185|0.42404|0.00258|0.88164|0.00514|29s

</div>

### [COSTA](../../../src/implementations/zslearn/costa.py)

In a similar way to ConSE, also this method uses a mathematical approach but instead of computing a semantic embedding used to rank the most likely unseen label, it directly compute and injects a classification head for each candidate label into the original model, producing a classification head with origin's input features and the number of candidate labels as output features.

The weights of an unseen label is computed as follows:

$$w_u = \sum_{s \in S}{w_s * c_{us}}, \forall u \in U$$

with $c_{us}$ being the co-occurrence similarity between label $u$ and $s$, computed as number of times the two labels are simultaneously relevant for each attribute, divided by the number of times $c_u$ is relevant for each attribute $a \in A$:

$$c_{us} = \frac{\sum_{a \in A}{a_u * a_s}}{\sum_{a \in A}{a_u}}$$

Although the above formula is perfectly compatible with non-binary semantic embeddings, the best performances are achieved by using the binary attributes obtained by running:
```sh
python ./scripts/data_preparation/make_attributes.py -d cub awa2 apy -m binary -n binatts
```
for then running the method with the [method's configuration file](../../../config/experiment/method/costa.yaml) parameters by executing the following command:
```sh
python ./main.py -m experiment=zslearn experiment/method=costa experiment.method.embeddings=binatts experiment/dataset=cub,awa2,apy
```

<div class="metrics-table">

**Dataset**|**ZSL Accuracy**|**GZSL Accuracy**|**GZSL Accuracy on $U$**|**GZSL Accuracy on $S$**|**Harmonic Accuracy**|**Run-Time**
:---:|:---:|:---:|:---:|:---:|:---:|:---:
APY|0.16481|0.42449|0.00084|0.88444|0.00172|29s
AWA2|0.14764|0.72559|0.00000|0.92063|0.00000|46s
CUB|0.01729|0.54258|0.00000|0.72524|0.00000|27s

</div>