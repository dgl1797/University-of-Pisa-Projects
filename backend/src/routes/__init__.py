from flask import Flask, redirect
from flask_cors import CORS
from flask.blueprints import Blueprint

def create_app():
  app = Flask(__name__)
  CORS(app, support_credentials=True)

  #imports
  from .home_router import home_router
  from .api_router import api_router

  #default routes
  @app.route("/")
  def nav():
    return redirect("/app")

  #blueprints registrations
  app.register_blueprint(home_router, url_prefix='/app')
  app.register_blueprint(api_router, url_prefix='/api')

  return app