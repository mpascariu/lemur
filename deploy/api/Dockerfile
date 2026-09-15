# syntax=docker/dockerfile:1

FROM python:3.13-slim

WORKDIR /app

COPY requirements.txt requirements.txt

RUN pip3 install --no-cache-dir -r requirements.txt

COPY . .

# CMD is provided by docker-compose (gunicorn --bind 0.0.0.0:5000 app:app)