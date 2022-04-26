#!/usr/bin/python3
from flask import Flask, request, jsonify
import endpoints

def create_app():
    app = Flask(__name__)
    app.config["DEBUG"] = True
    return app
app = create_app()

@app.route('/', methods=['GET'])
def home():
    return '''<h1>Leverhulme Center for Demographic Science</h1>
<p>A prototype API (v1) to infuse data science into demography.</p>'''

@app.errorhandler(404)
def page_not_found(e):
    return "<h1>404 Error</h1><p>The resource could not be found.</p>", 404


#---- EXAMPLE ENDPOINT ----#

@app.route('/the_verse', methods=['GET'])
def the_verse():
    result = the_verse_fun()
    return jsonify(result)


#---- SOCIAL MEDIA AUDIENCE ----#

@app.route('/social_media_audience/query', methods=['GET'])
def fb_query():
    """API endpoint to select data from the 'social_media_audience' database."""
    args = dict(request.args)
    if len(args) > 0:
        result = audience.query_fun(args)
        return jsonify(result), result.get("status")
    else:
        return "<h1>400 Error</h1><p>Bad Request: This API endpoint requires arguments. See <a href='http://10.131.129.27/api/social-media-audience.html#query'>API documentation</a> for more info.", \
               400

@app.route('/social_media_audience/query_ukraine', methods=['GET'])
def fb_query_ukraine():
    """API endpoint to select data from the 'social_media_audience' database."""
    args = dict(request.args)
    if len(args) > 0:
        result = audience.query_ukraine_fun(args)
        return jsonify(result), result.get("status")
    else:
        return "<h1>400 Error</h1><p>Bad Request: This API endpoint requires arguments. See <a href='http://10.131.129.27/api/social-media-audience.html#query_ukraine'>API documentation</a> for more info.", \
               400

@app.route('/social_media_audience/write', methods=['GET'])
def fb_write():
    """API endpoint to insert data into the 'social_media_audience' database."""
    args = dict(request.args)
    if len(args) > 0:
        result = audience.write_fun(args)
        return jsonify(result), result.get("status")
    else:
        return "<h1>400 Error</h1><p>Bad Request: This API endpoint requires arguments. See <a href='http://10.131.129.27/api/social-media-audience.html#write'>API documentation</a> for more info.", \
               400

@app.route('/social_media_audience/write_geo', methods=['GET'])
def fb_write_geo():
    result = audience.write_geo_fun(dict(request.args))
    return jsonify(result), result.get("status")


#---- GUN VIOLENCE ARCHIVE ----#

@app.route('/gun_violence_archive/write', methods=['GET'])
def gva_write():
    """API endpoint to insert data into the 'gva' database."""
    args = dict(request.args)
    if len(args) > 0:
        result = gva.write_fun(args)
        return jsonify(result), result.get("status")
    else:
        return "<h1>400 Error</h1><p>Bad Request: This API endpoint requires arguments. See <a href='http://10.131.129.27/api/gun-violence-archive.html#write'>API documentation</a> for more info.", \
               400

@app.route('/gun_violence_archive/getnext', methods=['GET'])
def gva_getnext():
    """Get next incident id not already collected."""
    args = dict(request.args)
    if len(args) == 0:
        args = {'n': 1}
    result = gva.getnext_fun(args)
    return jsonify(result), result.get("status")


if __name__ == '__main__':
    app.run()








