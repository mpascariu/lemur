#!/bin/bash

# install virtual environment
python3 -m venv venv

# activate virtual environment
. venv/bin/activate

# install flask
pip3 install flask
pip3 install psycopg2-binary

# document dependencies
pip freeze > requirements.txt

