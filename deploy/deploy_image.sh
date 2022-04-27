#!/bin/bash

# save lemur image to tar
docker save -o lemur.tar lemur_shiny

# transfer tar to server
scp lemur.tar ubuntu@lemur:~/lemur.tar

# rebuild image on server from tar
ssh lemur "docker load -i ~/lemur.tar"
