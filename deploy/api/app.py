#!/usr/bin/python3

from flask import Flask, request, jsonify, current_app
from api.endpoints import api_fun, regions_fun
from api.utils import query


def create_app():
    app = Flask(__name__)
    app.config["DEBUG"] = True
    return app


app = create_app()


@app.route("/", methods=["GET"])
def home():
    return current_app.send_static_file('docs.html')


@app.errorhandler(404)
def page_not_found(e):
    return "<h1>404 Error</h1><p>The resource could not be found.</p>", 404


# ---- ENDPOINTS ----#

@app.route("/cause_of_death", methods=["GET"])
def cod():
    """API endpoint to select data from the 'cod' table of the `gbd2019` database."""
    args = dict(request.args)
    if len(args) > 0:
        result = api_fun(args, table='cod', ip=str(request.remote_addr))
        return jsonify(result), result.get("status")
    else:
        return (
            "<h1>400 Error</h1><p>Bad Request: This API endpoint requires arguments.",
            400,
        )

@app.route("/life_table", methods=["GET"])
def lt():
    """API endpoint to select data from the 'lt' table of the `gbd2019` database."""
    args = dict(request.args)
    if len(args) > 0:
        result = api_fun(args, table='lt', ip=str(request.remote_addr))
        return jsonify(result), result.get("status")
    else:
        return (
            "<h1>400 Error</h1><p>Bad Request: This API endpoint requires arguments.",
            400,
        )

@app.route("/sdg", methods=["GET"])
def sdg():
    """API endpoint to select data from the 'sdg' table of the `gbd2019` database."""
    args = dict(request.args)
    if len(args) > 0:
        result = api_fun(args, table='sdg', ip=str(request.remote_addr))
        return jsonify(result), result.get("status")
    else:
        return (
            "<h1>400 Error</h1><p>Bad Request: This API endpoint requires arguments.",
            400,
        )

@app.route("/regions", methods=["GET"])
def regions():
    """API endpoint to return full list of regions from the `gbd2019` database."""
    result = regions_fun(ip=str(request.remote_addr))

    return jsonify(result), result.get("status")

if __name__ == "__main__":
    app.run()
