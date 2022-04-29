#!/usr/bin/python3

from flask import Flask, request, jsonify, current_app
from flask_limiter import Limiter
from flask_limiter.util import get_remote_address
from api.endpoints import api_fun, regions_fun, requests_fun


def create_app():
    app = Flask(__name__)
    app.config["DEBUG"] = True
    return app


app = create_app()
limiter = Limiter(app, key_func=get_remote_address)
rate_limit = "30/minute"


@app.errorhandler(404)
def page_not_found(e):
    return "<h1>404 Error</h1><p>The resource could not be found.</p>", 404


@app.route("/", methods=["GET"])
@limiter.limit(rate_limit)
def home():
    return current_app.send_static_file('docs.html')


# ---- ENDPOINTS ----#

@app.route("/cause_of_death", methods=["GET"])
@limiter.limit(rate_limit)
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
@limiter.limit(rate_limit)
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
@limiter.limit(rate_limit)
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
@limiter.limit(rate_limit)
def regions():
    """API endpoint to return full list of regions from the `gbd2019` database."""
    result = regions_fun(ip=str(request.remote_addr))

    return jsonify(result), result.get("status")


@app.route("/requests", methods=["GET"])
@limiter.limit(rate_limit)
def requests():
    """API endpoint to return counts of API requests to `gbd2019` database."""
    args = dict(request.args)
    result = requests_fun(date=args.get('date'))
    return result.get('html'), result.get("status")


if __name__ == "__main__":
    app.run()
