#!/bin/bash
#To run this deployment you will need to have capistrano and capistrano-ext installed

sudo apt-get install rubygems
sudo gem install lib/net-ssh-2.1.0.gem
sudo gem install lib/net-sftp-2.0.5.gem
sudo gem install lib/net-scp-1.0.4.gem
sudo gem install lib/net-ssh-gateway-1.0.1.gem
sudo gem install lib/highline-1.6.1.gem
sudo gem install lib/capistrano-2.5.19.gem
sudo gem install lib/capistrano-ext-1.2.1.gem
