# lemur server config


# update
sudo apt-get update 
sudo apt-get upgrade

# packages
sudo apt install git
sudo apt install nginx
sudo apt-get install ca-certificates curl gnupg lsb-release
sudo apt-get install docker-ce docker-ce-cli containerd.io
sudo apt-get install docker-compose
sudo apt-get install r-base

#---- swap ----#
sudo fallocate -l 16G /swapfile
sudo chmod 600 /swapfile
sudo mkswap /swapfile
sudo swapon /swapfile
swapon --show
sudo cp /etc/fstab /etc/fstab.back
echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab
sudo sysctl vm.swappiness=1

sudo nano /etc/sysctl.conf
# add line: vm.swappiness=1

#---- ufw ----#
sudo ufw default deny incoming
sudo ufw default allow outgoing
sudo ufw allow from 163.1.150.0/24 proto tcp to any port 22
sudo ufw allow 'Nginx Full'

sudo ufw enable

#---- nginx ----#
sudo systemctl start nginx
sudo cp /etc/nginx/sites-available/default /etc/nginx/sites-available/apps.conf
sudo ln -s /etc/nginx/sites-available/apps.conf /etc/nginx/sites-enabled/apps.conf

#---- docker ----#

# build docker
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg

echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
  
# add user to docker group
sudo usermod -aG docker ubuntu
newgrp docker


#---- shinyproxy ----#

# modify docker config
sudo systemctl edit docker
# [Service]
# ExecStart=
# ExecStart=/usr/bin/dockerd -H unix:// -D -H tcp://127.0.0.1:2375

sudo systemctl daemon-reload
sudo systemctl restart docker

# # pull example dockerized shiny app
# docker pull openanalytics/docker-example

# create docker network for shinyproxy
sudo docker network create shiny-net


#---- lemur app ----#

# NOTE: first setup a GitHub deploy key for the lemur repository and add it to ~/.ssh/config

mkdir ~/git
cd ~/git
git clone git@github.com:doug-leasure/lemur












