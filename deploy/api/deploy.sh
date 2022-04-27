#!/bin/bash

docker stop api-ukraine
docker rm api-ukraine
docker build --tag api-ukraine .

docker run -d -p 127.0.0.1:5002:5000 --env SQLHOST --env SQLUSER --env SQLPASS --restart=always --name api-ukraine api-ukraine

docker logs api-ukraine
