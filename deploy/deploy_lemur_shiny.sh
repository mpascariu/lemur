#!/bin/bash

cd $HOME/git/doug-leasure/lemur/

# build image
docker build --tag lemur_shiny .

# save lemur image to tar
docker save -o ./deploy/lemur.tar lemur_shiny

# transfer tar to server
scp ./deploy/lemur.tar ubuntu@lemur:~/lemur.tar

# rebuild image on server from tar
ssh lemur "docker load -i ~/lemur.tar"

# reload docker container
ssh lemur "cd /home/ubuntu/git/lemur;docker-compose up -d"
