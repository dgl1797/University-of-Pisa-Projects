from access_points import QdrantQueryThread, encoder
from config import OPENAI_API_KEY, OPENAI_ORGANIZATION
from torch import sum, clamp, Tensor
import spacy
import nltk
import openai

nltk.download('stopwords')
from nltk.corpus import stopwords
load_model = spacy.load('en_core_web_lg', disable = ['parser','ner'])

# def mean_pooling(model_output, attention_mask):
#   token_embeddings = model_output[0] #First element of model_output contains all token embeddings
#   input_mask_expanded = attention_mask.unsqueeze(-1).expand(token_embeddings.size()).float()
#   return sum(token_embeddings * input_mask_expanded, 1) / clamp(input_mask_expanded.sum(1), min=1e-9)

# def take_encoding(query: str) -> Tensor:
#   tokens = tokenizer(query, padding=True, truncation=True, return_tensors='pt', return_attention_mask=True)
#   embeddings = mean_pooling(transformer(**tokens), tokens['attention_mask'])
#   return embeddings

def clean_text(text):
    cleaned_text = ""
    tokens = nltk.word_tokenize(text)
    stop_words = set(stopwords.words('english'))
    filtered_sentence = []
    for w in tokens:
        if w not in stop_words:
            filtered_sentence.append(w)
    text_to_lemmatize = load_model(" ".join(filtered_sentence))
    cleaned_text=" ".join([token.lemma_ for token in text_to_lemmatize])
    return cleaned_text

def weighted_query(query, page, title_weight=1, summaries_weight=0.6):
  query_embeddings = encoder.encode(query)

  titles_query = QdrantQueryThread(collection_name="paper_titles", page=page, query_vector=query_embeddings)
  summaries_query = QdrantQueryThread(collection_name="paper_summaries", page=page, query_vector=query_embeddings)

  titles_query.start()
  summaries_query.start()

  titles_result = titles_query.join()
  summaries_result = summaries_query.join()

  final_rank = {}
  for doc in titles_result:
    final_rank[doc.payload["link"]] = title_weight * doc.score
  
  for doc in summaries_result:
    l = doc.payload["link"]
    s = summaries_weight * doc.score
    final_rank[l] = final_rank[l]+s if l in final_rank else s
  
  final_rank = sorted(final_rank.items(), key=lambda i: i[1], reverse=True)
  return [l[0] for l in final_rank]

def single_query(query, page, collection_name="st_finetuned_papers_abstract"):
  '''
  Title + Abstract: st_finetuned_papers_abstract (default)
  Title + Summary: st_finetuned_papers_summary
  '''
  query_embeddings = encoder.encode(query)

  simple_query = QdrantQueryThread(collection_name=collection_name, page=page, query_vector=query_embeddings)
  simple_query.start()
  payloads_response = simple_query.join()

  return [doc.payload["link"] for doc in payloads_response]

def openaiRequest(prompt: str) -> str:
  response = openai.ChatCompletion.create(
    api_key=OPENAI_API_KEY,
    organization=OPENAI_ORGANIZATION,
    model="gpt-3.5-turbo-16k",
    messages=[{"role": "user", "content": prompt}],
    max_tokens=8000,
    temperature=0.8
  )["choices"][0]["message"]["content"]
  print(response)
  return response

