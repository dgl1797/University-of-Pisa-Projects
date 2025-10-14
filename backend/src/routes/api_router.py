from flask import Blueprint, request, jsonify
import json

api_router = Blueprint("api", __name__)

@api_router.route("/cards", methods=["GET", "POST"])
def cardsController():
  if request.method == "GET":
    try:
      from .api_handlers import getCardList
      cardList = getCardList()
      return jsonify(cardList)
    except:
      return jsonify({"code": 500, "message": "Internal Error"})

@api_router.route("/suggestions", methods=["GET"])
def suggestionsController():
  try:
    card1 = request.args.get("c1")
    card2 = request.args.get("c2")
    card3 = request.args.get("c3")
    card4 = request.args.get("c4")
    card5 = request.args.get("c5")
    card6 = request.args.get("c6")
    card7 = request.args.get("c7")
    card8 = request.args.get("c8")

    deck = [card1, card2, card3, card4, card5, card6, card7, card8]

    from .api_handlers import getSuggestionList
    from .api_handlers import getCardSuggestedList

    suggestionList = getSuggestionList([card1, card2, card3, card4, card5, card6, card7, card8])

    cardList = getCardSuggestedList(suggestionList)

    return jsonify(cardList)
  except Exception as err:
    return jsonify({"code": 500, "message": str(err)})


@api_router.route("/base_classification", methods=["GET"])
def base_classificationController():
  try:
    card1 = request.args.get("c1")
    card2 = request.args.get("c2")
    card3 = request.args.get("c3")
    card4 = request.args.get("c4")
    card5 = request.args.get("c5")
    card6 = request.args.get("c6")
    card7 = request.args.get("c7")
    card8 = request.args.get("c8")

    deck = [card1, card2, card3, card4, card5, card6, card7, card8]

    from .api_handlers import getBasicRank

    final_rank = getBasicRank(deck)

    return jsonify(final_rank[0])
  except Exception as err:
    return jsonify({"code": 500, "message": err})

@api_router.route("/final_rank", methods=["GET"])
def final_rankController():
  try:
    card1 = json.loads(request.args.get("c1"))
    card2 = json.loads(request.args.get("c2"))
    card3 = json.loads(request.args.get("c3"))
    card4 = json.loads(request.args.get("c4"))
    card5 = json.loads(request.args.get("c5"))
    card6 = json.loads(request.args.get("c6"))
    card7 = json.loads(request.args.get("c7"))
    card8 = json.loads(request.args.get("c8"))
    deck = [card1, card2, card3, card4, card5, card6, card7, card8]

    from .api_handlers import getFinalRank

    result = getFinalRank(deck)

    return jsonify(result)
  except Exception as err:
    return jsonify({"code": 500, "message": err})