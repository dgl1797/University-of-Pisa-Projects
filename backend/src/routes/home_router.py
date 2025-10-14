from flask import Blueprint, redirect

home_router = Blueprint("home", __name__)

@home_router.route('/', methods=["GET"])
def home_controller():
  return "<h1>Hello APP</h1>"