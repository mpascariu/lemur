#!/usr/bin/python3
from flask import Flask, request, jsonify
from api.endpoints import cod_fun, lt_fun, sdg_fun


def create_app():
    app = Flask(__name__)
    app.config["DEBUG"] = True
    return app


app = create_app()


@app.route("/", methods=["GET"])
def home():
    return """<h1>lemur: Life Expectancy Monitor</h1>
<p>Monitor life expectancy and causes of mortality for specific demographic groups in every country globally.</p>"""


@app.errorhandler(404)
def page_not_found(e):
    return "<h1>404 Error</h1><p>The resource could not be found.</p>", 404


# ---- ENDPOINTS ----#


@app.route("/cause_of_death", methods=["GET"])
def query():
    """API endpoint to select data from the 'cod' table of the `gbd2019` database."""
    args = dict(request.args)
    if len(args) > 0:
        result = cod_fun(args)
        return jsonify(result), result.get("status")
    else:
        return (
            "<h1>400 Error</h1><p>Bad Request: This API endpoint requires arguments.",
            400,
        )

@app.route("/life_table", methods=["GET"])
def query():
    """API endpoint to select data from the 'lt' table of the `gbd2019` database."""
    args = dict(request.args)
    if len(args) > 0:
        result = lt_fun(args)
        return jsonify(result), result.get("status")
    else:
        return (
            "<h1>400 Error</h1><p>Bad Request: This API endpoint requires arguments.",
            400,
        )

@app.route("/sdg", methods=["GET"])
def query():
    """API endpoint to select data from the 'sdg' table of the `gbd2019` database."""
    args = dict(request.args)
    if len(args) > 0:
        result = sdg_fun(args)
        return jsonify(result), result.get("status")
    else:
        return (
            "<h1>400 Error</h1><p>Bad Request: This API endpoint requires arguments.",
            400,
        )


if __name__ == "__main__":
    app.run()
