from flask.json import jsonify
from pymongo import MongoClient
import json
import numpy as np
import pandas as pd
from sklearn.pipeline import Pipeline
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_selection import SelectKBest, chi2

def replace_frozenset(x):
  modified_string = x.replace('frozenset','').replace('{','').replace('}', '').replace('(', '').replace(' ', '').replace(')', '').replace("'",'')
  return [item for item in modified_string.split(',')]

def getCardList():
  client = MongoClient("mongodb://localhost:27017")
  db = client.DataMining
  collection = db.cards
  cardList = json.loads(json.dumps([card for card in collection.find({})], default=str))
  client.close()
  return cardList

def getSuggestionList(deck):
  rulesDF = pd.read_csv("D:/User/Desktop/Work/UniPI/DataMining/DeckBuilder/backend/csv_sources/parsed_rules.csv", 
  converters={
    'antecedents': lambda x: replace_frozenset(x),
    'consequents': lambda y: replace_frozenset(y)
  }).drop(['Unnamed: 0'], axis='columns')

  suggestions_set = []

  for card in deck:
    found_match = [rule for rule in rulesDF.values if card in rule[0]]
    for fm in found_match:
      for c in fm[1]:
        suggestions_set.append(c)
  
  unique_suggestions = np.unique(suggestions_set)
  result = [x for x in unique_suggestions]
  return result

def getCardSuggestedList(suggestionList):
  client = MongoClient("mongodb://localhost:27017")
  db = client.DataMining
  collection = db.cards
  if len(suggestionList) == 0:
    cardList = json.loads(json.dumps([c for c in collection.find({})], default=str))
  else:
    cardList = json.loads(json.dumps([card for card in collection.find({
      "CID": {"$in": [int(x) for x in suggestionList]}
    })], default=str))
  client.close()
  return cardList


def analyze_deck(deck):
  rulesDF = pd.read_csv("D:/User/Desktop/Work/UniPI/DataMining/DeckBuilder/backend/csv_sources/parsed_rules.csv", 
  converters={
    'antecedents': lambda x: replace_frozenset(x),
    'consequents': lambda y: replace_frozenset(y)
  }).drop(['Unnamed: 0'], axis='columns')

  total_conf = 0
  total_lift = 0
  for i in range(len(deck)):
    found_match = [x for x in rulesDF.values if deck[i] in x[0]]
    if len(found_match) > 0:
      for j in range(len(deck)):
        if (i == j):
          continue
        final_rule = [y for y in found_match if deck[j] in y[1]]
        if len(final_rule) > 0:
          for fm in found_match:
            total_conf+=fm[5]
            total_lift+=fm[6]
  return (total_conf, total_lift)

def converter(deck, leveled=False):
  levelSum = 0
  commonLevelSum = 0
  rareLevelSum = 0
  epicLevelSum = 0
  legendaryLevelSum = 0
  troop_count = 0
  avgSpeed = 0
  avgSpellDuration = 0
  totalHP = 0
  totalDMG = 0
  avgAS = 0
  totalUnitsCount = 0
  abilityCount = 0
  avgRange = 0
  totalGroundT = 0
  totalAirT = 0
  totalBuildingT = 0
  totalBuffCount = 0
  totalProjectiles = 0
  buildingCount = 0
  spellCount = 0
  commonCount = 0
  rareCount = 0
  epicCount = 0
  legendaryCount = 0
  elixirCount = 0
  myDict = {}
  
  for card in deck:
    if leveled:
      levelSum += int(card["level"], 10)
    if card["rarity"] == "Common":
      commonCount += 1
      if leveled:
        commonLevelSum += int(card["level"], 10)
    elif card["rarity"] == "Rare":
      rareCount += 1
      if leveled:
        rareLevelSum += int(card["level"], 10)
    elif card["rarity"] == "Epic":
      epicCount += 1
      if leveled:
        epicLevelSum += int(card["level"], 10)
    else:
      legendaryCount += 1
      if leveled:
        legendaryLevelSum += int(card["level"], 10)
    elixirCount += card['elixir']
    if card['type'] == 'Troop':
      troop_count+=1
      avgSpeed += card["speed"]
      totalHP += card["hitpoints"][len(card["hitpoints"])-1]
      totalDMG += card["damage"][len(card["damage"])-1]
      avgAS += card["hps"]
      avgRange += card["range"]
      totalUnitsCount += card["count"]
      if card["ability"]:
        abilityCount += 1
      if card["attacks_ground"]:
        totalGroundT += 1
      if card["attacks_air"]:
        totalAirT += 1
      if card["target_only_buildings"]:
        totalBuildingT += 1
    if card["type"] == "Spell":
      spellCount += 1
      if "duration" in card:
        if card["duration"] != None:
          avgSpellDuration += card["duration"]
      avgRange += card["radius"]
      if card["hits_ground"]:
        totalGroundT += 1
      if card["hits_air"]:
        totalAirT += 1
      if "appliesBuff" in card:
        if card["appliesBuff"]:
          totalBuffCount += 1
      if card["projectile"]:
        totalProjectiles += 1
      if "damage" in card:
        totalDMG += card["damage"][len(card["damage"])-1]
      if "ignore_buildings" in card:
        if card["ignore_buildings"]:
          totalBuildingT += 1
    if card["type"] == "Building":
      avgSpellDuration += card["duration"]
      avgRange += card["range"]
      totalHP += card["hitpoints"][len(card["hitpoints"])-1]
      if "damage" in card:
        if card["damage"] != None:
          totalDMG += card["damage"][len(card["damage"])-1]
      if "hitspeed" in card:
        if card["hitspeed"] != None:
          avgAS += card["hitspeed"]
      if card["attacks_ground"]:
        totalGroundT += 1
      if card["attacks_air"]:
        totalAirT += 1
  # end for 
  if(troop_count > 0):
    avgSpeed = avgSpeed / troop_count
  if (troop_count + buildingCount > 0): 
    avgAS = avgAS / (troop_count + buildingCount)
  if (spellCount + buildingCount > 0):
    avgSpellDuration = avgSpellDuration / (spellCount + buildingCount)
  avgRange = avgRange / 8
  elixirCount = elixirCount / 8

  total_conf, total_lift = analyze_deck(deck)

  myDict["troop"] = [troop_count]
  myDict["building"] = [buildingCount]
  myDict["spell"] = [spellCount]
  myDict["common"] = [commonCount]
  myDict["rare"] = [rareCount]
  myDict["epic"] = [epicCount]
  myDict["legendary"] = [legendaryCount]
  myDict["elixir"] = [elixirCount]
  myDict["conf_synergy"] = [total_conf]
  myDict["lift_synergy"] = [total_lift]
  if leveled == True:
    myDict["levelSum"] = [levelSum]
  myDict["avgSpeed"] = [avgSpeed]
  myDict["avgSpellDuration"] = [avgSpellDuration]
  myDict["totalHP"] = [totalHP]
  myDict["totalDMG"] = [totalDMG]
  myDict["avgAS"] = [avgAS]
  myDict["totalUnitsCount"] = [totalUnitsCount]
  myDict["abilityCount"] = [abilityCount]
  myDict["avgRange"] = [avgRange]
  myDict["totalGroundT"] = [totalGroundT]
  myDict["totalAirT"] = [totalAirT]
  myDict["totalBuildingT"] = [totalBuildingT]
  myDict["totalBuffCount"] = [totalBuffCount]
  myDict["totalProjectiles"] = [totalProjectiles]
  if leveled == True:
    if (commonCount > 0):
        myDict["commonLevel"] = commonLevelSum / commonCount
    else:
        myDict["commonLevel"] = 0
    if rareCount > 0:
        myDict["rareLevel"] = rareLevelSum / rareCount
    else:
        myDict["rareLevel"] = 0
    if epicCount > 0:
        myDict["epicLevel"] = epicLevelSum / epicCount
    else:
        myDict["epicLevel"] = 0
    if legendaryCount > 0:
        myDict["legendaryLevel"] = legendaryLevelSum / legendaryCount
    else:
        myDict["legendaryLevel"] = 0

  return pd.DataFrame(myDict)
    

def getBasicRank(deck):
  client = MongoClient("mongodb://localhost:27017")
  db = client.DataMining
  collection = db.cards
  cardList = json.loads(json.dumps([card for card in collection.find({
    "CID": {"$in": [int(x) for x in deck]}
  })], default=str))

  converted_list = converter(cardList)

  X_data = np.array([x for x in converted_list.values])

  ranks_df = pd.read_csv("D:/User/Desktop/Work/UniPI/DataMining/DeckBuilder/backend/csv_sources/ranks_unleveled.csv").drop(["index", "levelSum", "crowns"], axis="columns")
  
  y = np.array([x for x in ranks_df["rank"]])
  X = np.array([x for x in ranks_df.drop(["rank"], axis="columns").values])

  pip = Pipeline([
    ('feat_sel', SelectKBest(chi2, k=20)),
    ('clf', RandomForestClassifier(n_estimators=100))
  ])

  fitted_pip = pip.fit(X,y)

  classification = fitted_pip.predict(X_data)

  return classification

def getFinalRank(deck):
  client = MongoClient("mongodb://localhost:27017")
  db = client.DataMining
  collection = db.cards
  cardList = json.loads(json.dumps([card for card in collection.find({
    "CID": {"$in": [int(x["CID"]) for x in deck]}
  })], default=str))

  for ind in range(len(cardList)):
    mycard = [c for c in deck if cardList[ind]["CID"] == c["CID"]][0]
    cardList[ind]["level"] = mycard["level"]
  
  converted_list = converter(cardList, leveled=True)

  X_data = np.array([x for x in converted_list.values])

  ranks_df = pd.read_csv("D:/User/Desktop/Work/UniPI/DataMining/DeckBuilder/backend/csv_sources/leveled_for_rarity.csv").drop(["index", "crowns"], axis="columns")
  
  y = np.array([x for x in ranks_df["rank"]])
  X = np.array([x for x in ranks_df.drop(["rank"], axis="columns").values])

  pip = Pipeline([
    ('clf', RandomForestClassifier(n_estimators=200))
  ])

  fitted_pip = pip.fit(X, y)

  classification = fitted_pip.predict(X_data)

  return [d for d in classification][0]