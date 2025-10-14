import math
import mmh3

class BloomFilter(object):

  def __init__(self, n, p):
    log2squared = math.pow(math.log(2), 2)
    m_full = -((n * math.log(p)) / (log2squared))
    m = math.ceil(m_full)
    k_full = m / n * math.log(2)
    k = math.ceil(k_full)
    self.nBits = m
    self.nHash = k
    self.filter = [False]*self.nBits
  
  def __str__(self) -> str:
    return f"n={self.nBits}; k={self.nHash}"
    
  def add(self, word):
    indexes = [(mmh3.hash(word, i, False) % self.nBits) for i in range(self.nHash)]
    for i in indexes:
      self.filter[i] = True
  
  def check(self, word):
    indexes = [(mmh3.hash(word, i, False) % self.nBits) for i in range(self.nHash)]
    for i in indexes:
      if self.filter[i] == False:
        return False
    return True
