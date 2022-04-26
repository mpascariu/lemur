#!/bin/bash

# build image
docker build --tag lemur $HOME/git/doug-leasure/lemur/

# save image to tar
docker save -o $HOME/git/doug-leasure/lemur/deploy/lemur.tar lemur

# transfer image to server
scp $HOME/git/doug-leasure/lemur/deploy/lemur.tar ubuntu@lemur:/home/ubuntu/git/lemur/deploy/lemur.tar



