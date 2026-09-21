# lemur server config


# update
sudo apt-get update 
sudo apt-get upgrade

# packages
sudo apt install git
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
sudo ufw allow http
sudo ufw allow https

sudo ufw enable


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

# ShinyProxy needs access to /var/run/docker.sock to start app containers.
# Access is granted by group, via group_add in docker-compose.yml using the
# DOCKER_GID value from .env. Find the numeric group id with:
#   stat -c %g /var/run/docker.sock
#
# Never make the socket world-writable (chmod a+rw): dockerd recreates it
# with 0660 root:docker on every restart, the change evaporates, and every
# new visitor session then fails with 500 "Container failed to start".
# The old advice to run dockerd with -H tcp://127.0.0.1:2375 is also gone:
# ShinyProxy talks to the unix socket only, there is no reason to expose
# a TCP endpoint.


#---- lemur app ----#

# NOTE: first setup a GitHub deploy key for the lemur repository and add it to ~/.ssh/config

mkdir ~/git
cd ~/git
git clone git@github.com:mpascariu/lemur

# deploy
cd ~/git/lemur
docker compose up -d












