import os
from pathlib import Path

from dotenv import load_dotenv

load_dotenv()

# Paths
ROOT = Path(__file__).parent
DATA = ROOT / "data"
MAX_SENTENCE_LENGTH = 100

# Qdrant
QDRANT_HOST = os.getenv("QDRANT_HOST")
QDRANT_PORT = os.getenv("QDRANT_PORT")
QDRANT_API_KEY = os.getenv("QDRANT_API_KEY")
HOSTNAME = os.getenv("HOST_NAME")
HOSTPORT = os.getenv("PORT")
COLLECTION_NAME = "papers"

# OpenAI
OPENAI_API_KEY = os.getenv("OPENAI_API_KEY")
OPENAI_ORGANIZATION = os.getenv("OPENAI_ORGANIZATION")

# Server
ALLOWED_ORIGINS = os.getenv("ALLOWED_ORIGINS")