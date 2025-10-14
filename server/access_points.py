from qdrant_client import QdrantClient
from config import HOSTNAME, QDRANT_HOST, QDRANT_PORT, QDRANT_API_KEY
from sentence_transformers import SentenceTransformer
from pymongo import MongoClient
from threading import Thread
import torch

qdrand_client = QdrantClient(host=QDRANT_HOST, port=QDRANT_PORT, api_key=QDRANT_API_KEY)
mongo_client = MongoClient(f'mongodb://{HOSTNAME}:{27017}').get_database('scientific_articles')

mongo_articles = mongo_client.get_collection('articles')

encoder = SentenceTransformer('sentence-transformers/allenai-specter').to("cuda" if torch.cuda.is_available() else "cpu")
encoder.eval()

class QdrantQueryThread(Thread):
  def __init__(self, collection_name: str = "papers_complete", page: int = 0, query_vector: list[float] = []):
    Thread.__init__(self)
    self.__collection_name = collection_name
    self.__page = page
    self.__query_vector = query_vector 
    self.__return = None
  
  def run(self):
    self.__return = qdrand_client.search(
      collection_name=self.__collection_name, query_vector=self.__query_vector, limit=6, offset=self.__page*6, with_payload=True
    )
  
  def join(self):
    Thread.join(self)
    return self.__return