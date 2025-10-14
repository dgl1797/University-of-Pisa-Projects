# Concept
The aim of the project is to build a bloom filter over the ratings of movies listed in the IMDb datasets. A bloom filter is a space and time efficient data structure that exploits statistics to early-determine whether a specific data is within the dataset or not. Because of its stochastic nature, it may give false positives (an instance is recorded as present even though it is not) but it necessarily avoids false negatives (an instance is recorded as absent even though it is present within the dataset) 

The overall job is divided into 3 main area: Design, Implementation and Results. Firstly, the design phase involves the creation of the MapReduce algorithm, presented using a pseudocode. These instructions are useful to implement the bloom filters.

Then, it follows the implementation of the already discussed algorithm using both
the Hadoop and the Spark framework, separately.

Lastly, a test phase for both the implementations on the IMDb ratings dataset, computing the exact number of false positives for each rating, has been executed on a cluster of 4 virtual machines provided by the University of Pisa.