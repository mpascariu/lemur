# LCDS Application Programming Interface for Ukraine Social Watcher project

This is an Application Programming Interface (API) that can be used to read from the LCDS PostgreSQL server. It is written in Python using the Flask package, and it is designed to be deployed as a Docker container behind an nginx web server.

## Local Usage

Setup a virtual environment:

```
# install virtual environment
python3 -m venv venv

# activate virtual environment
. venv/bin/activate

# install requirements with pip
pip3 install -r requirements.txt
```

The API can then be launched independently (i.e. without Docker and Apache) using:

```
python ./app.py
```

The API can be launched as a Docker container using the shell script `./deploy.sh`.
