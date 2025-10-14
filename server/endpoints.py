import os
from typing import Union
from fastapi import FastAPI
from fastapi.encoders import jsonable_encoder
from fastapi.responses import JSONResponse
from fastapi.middleware.cors import CORSMiddleware
from http import HTTPStatus
from schemes import ErrorScheme, SearchResponse, SummaryBody
from access_points import QdrantQueryThread, mongo_articles, encoder
from logic import weighted_query, openaiRequest, single_query
from config import ALLOWED_ORIGINS
from torch import squeeze
import threading
import json
import requests

app = FastAPI()
app.add_middleware(CORSMiddleware, allow_origins=ALLOWED_ORIGINS, allow_methods=["*"])

@app.get("/search")
def get_search(query: str, page: int):

  links = single_query(query=query, page=page)

  parsings = mongo_articles.find({
    "link":{
      "$in": links
    }
  })

  final_response = [SearchResponse(
    doc["title"], 
    doc["publication_date"], 
    doc["summary"], 
    doc["link"], 
    doc["authors"]
  ) for doc in parsings if "summary" in doc]

  return JSONResponse(jsonable_encoder(ErrorScheme("frek't", HTTPStatus.BAD_REQUEST))) if query == ""\
    else JSONResponse(jsonable_encoder(final_response))

@app.put("/summary")
def get_summary(body: SummaryBody):
  links = body.links
  query = body.query

  parsings = mongo_articles.find({
    "link":{
      "$in": links
    }
  })
  results = list(parsings)

  prompt = f'You have been asked: "{query}" and you have information about the following papers summaries:\n'
  for idx, doc in enumerate(results):
    prompt += f'index: {idx}; title: {doc["title"]}; summary: {doc["summary"]}\n'
    continue
  prompt +=  f'.\nReferencing the provided articles with [index], \
    Generate a 4000 word article as a single json document with the following keys:\n \
    Title, Content.' 

  unprocessed_answer = json.loads(openaiRequest(prompt), strict=False)
  answer = {}

  for k in unprocessed_answer:
    newkey = k.lower()
    answer[newkey] = unprocessed_answer[k] if newkey not in answer else answer[newkey] + unprocessed_answer[k]
  
  del unprocessed_answer

  with open('answer.json', "w") as jsonfile:
    jsonfile.write(json.dumps(answer))
  
  section_text = f'<h1 style="text-align:center;">{answer["title"] if "title" in answer else "Mannaggia a ChatGPT"}</h1>'
  empty_stack = True
  for section in answer:
    empty_stack = False
    if section == "title":
      continue
    section_text += f'<h2>{section.upper()}</h2>'
    section_text += f'<p>{answer[section]}'
  
  section_text += "<h2>REFERENCES</h2>"
  for idx, doc in enumerate(results):
    section_text += f'<p>[{idx}]\t<a href={doc["link"]} target="_blank">{doc["title"]}</a></p>'    

  return section_text if not empty_stack else jsonable_encoder(ErrorScheme("No results found, the issue may be caused by an unwanted page reload, reloading the page in this stage of the application may cause a wrong format for the request", HTTPStatus.BAD_REQUEST))