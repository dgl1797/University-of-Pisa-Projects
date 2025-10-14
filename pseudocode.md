# RATINGS INSTANCES COUNTING

```python
class MAPPER:
  method MAP(chunkid cid, chunk c):
    foreach line in c:
      int vote <- round(c[1])
      int n <- 1
      EMIT(vote, n)

class REDUCER:
  method REDUCE(int vote, counts [c1, c2 ...]):
    int total <- sum(counts)
    EMIT(vote, total)
```

# BLOOM FILTERS CONSTRUCTION

```python
class MAPPER:
  method MAP(chunkid cid, chunk c):
    foreach line in c:
      int vote <- round(c[1])
      str title <- c[0]
      EMIT(vote, title)

class REDUCER:
  method REDUCE(int vote, counts [t1, t2 ...]):
    bloomfilter bf <- new bloomfilter(n,p)
    file f <- new file(outputpath+"filter_"+vote)
    foreach el in counts:
      bf.add(el)
    write(f, bf)

class BLOOMFILTER:
  int nbits
  int nhash
  hash[nhash] h
  boolean bitfield[nbits]

  constructor(int num_entries, double pFP):
    m <- %formula%
    k <- %formula%
    this.nbits <- m
    this.nhash <- k
    foreach i in [0,h-1]:
      h <- new murmurhash

  add(string word):
    foreach hashf in h:
      bitfield[hashf(word)] <- True

  check(string word):
    foreach hashf in h:
     if bitfield[hashf(word)] == False:
       return False
    return True
```

# BLOOM FILTERS TESTER

```python
class MAPPER:
  method MAP(chunkid cid, chunk c):
    foreach line in c:
      int vote <- round(c[1])
      str title <- c[0]
      EMIT(vote, title)

class REDUCER:
  method REDUCE(int vote, counts [t1, t2 ...]):
    foreach el in counts:
      foreach i in [1,10]:
        if bloomfilter[i].check(el) == True:
          if i == vote:
            true_positive[i]+=1
          else:
            false_positive[i]+=1
        else:
          if i != vote:
            true_negative[i]+=1
          else:
            false_negative[i]+=1
    write(output_file, results)
```
