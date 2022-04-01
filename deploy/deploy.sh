#!/bin/bash

# nginx
sudo cp -a $HOME/git/lemur/deploy/apps.conf /etc/nginx/sites-available/apps.conf
sudo systemctl reload nginx

# shinyproxy
docker stop shinyproxy
docker rm shinyproxy
docker build --tag shinyproxy-lcds $HOME/git/lemur/deploy/

docker run -d -v /var/run/docker.sock:/var/run/docker.sock --restart=always --group-add $(getent group docker | cut -d: -f3) --net shiny-net -p 127.0.0.1:8080:8080 --name shinyproxy shinyproxy-lcds

# lemur
docker build --tag lemur $HOME/git/doug-leasure/lemur/


