from time import time
from colorama import Fore
from pyspark import SparkContext
from bloomfilter import BloomFilter
import sys

# UTILITY
print(Fore.WHITE+"")

def spark_print(text, title="SPARK", color=Fore.GREEN, looped=True):
  print(color+f"---------{title}--------\n")
  for t in text:
    if looped:
      print(*t, end="\n")
    else:
      print(t, end="\n")
  print(color+"\n---------END--------\n")
  print(Fore.WHITE)

def spark_separate(jobn: int):
  print(Fore.GREEN+f"-----------------------------------JOB {jobn}-----------------------------------"+Fore.WHITE)

def spark_timer_print(value: int):
  print("\n"+Fore.RED+f"Computation time: {value}s"+Fore.WHITE+"\n")
     
spark_print(text=["arg: "+sys.argv[i] for i in range(len(sys.argv))], title="ARGUMENTS",\
   color=Fore.LIGHTRED_EX, looped=False)

# IMPLEMENTATION
sc = SparkContext(appName="BLOOM_FILTER", master="yarn")
rdd_file = sc.textFile("hdfs://hadoop-namenode:9820/project/input/title_ratings.tsv")

# SHARED VARIABLES
filters_probability = sc.broadcast(float(sys.argv[1]))
quantities = [sc.accumulator(0) for _ in range(10)]
true_positives = [sc.accumulator(0) for _ in range(10)]
true_negatives = [sc.accumulator(0) for _ in range(10)]
false_positives = [sc.accumulator(0) for _ in range(10)]
false_negatives = [sc.accumulator(0) for _ in range(10)]

# WORKERS FUNCTIONS
def fileParser(x):
  arr = x.split("\t")
  vote = round(float(arr[1]))
  return (vote, arr[0])

def buildFilters(x, quants):
  vote = x[0]
  bf = BloomFilter(quants[vote-1], filters_probability.value)
  for movie in x[1]:
    bf.add(movie)
  return (vote, bf)

def testFilters(x, bfs):
  arr = x.split("\t")
  vote = round(float(arr[1]))
  movie = arr[0]
  for bf in bfs:
    filter_vote = bf[0]
    if bf[1].check(movie):
      if filter_vote == vote:
        true_positives[filter_vote-1].add(1)
      else:
        false_positives[filter_vote-1].add(1)
    else:
      if filter_vote != vote:
        true_negatives[filter_vote-1].add(1)
      else:
        false_negatives[filter_vote-1].add(1)

# JOB 1: COUNTING EACH VOTE'S ELEMENTS
spark_separate(1)
start = time()
rdd_file.foreach(lambda x: quantities[round(float(x.split("\t")[1])) - 1].add(1))
end = time()
spark_timer_print(end-start)
spark_print(quantities, "DIMENSIONS", color=Fore.YELLOW, looped=False)

# JOB 2: BUILDING FILTERS
spark_separate(2)
start = time()
quants = [quantities[i].value for i in range(len(quantities))]
bfs = rdd_file.map(lambda x: fileParser(x)).groupByKey()\
  .map(lambda x: buildFilters(x, quants)).collect()
end = time()
spark_timer_print(end-start)

# JOB 3: TESTING FILTERS
spark_separate(3)
start = time()
rdd_file.foreach(lambda x: testFilters(x, bfs))
end = time()
spark_timer_print(end-start)
print(Fore.GREEN+f"-----------------------------------RESULTS-----------------------------------")
for i in range(10):
  print(f"FILTER {i+1}: TP={true_positives[i].value}; TN={true_negatives[i].value}; FP={false_positives[i].value}\
    ; FN={false_negatives[i].value}")
print("-------------------------------------END-------------------------------------"+Fore.WHITE)