#!/bin/bash

# get prereqs
# setup oracle java
sudo add-apt-repository -y ppa:webupd8team/java
sudo apt-get update
sudo apt-get -y -qq install oracle-java7-installer
sudo apt-get -y -qq install scala git mosh byobu

# setup keys -- these keys are already added on github
ssh-keygen -q -t rsa -f ~/.ssh/id_rsa -N ""
rm .ssh/id_rsa*
wget http://www.cs.berkeley.edu/~shaddi/bdm/id_rsa
wget http://www.cs.berkeley.edu/~shaddi/bdm/id_rsa.pub
mv id_rsa* .ssh/.
chmod og-rwx .ssh/id_rsa
echo -e "Host github.com\n\tStrictHostKeyChecking no\n" >> ~/.ssh/config

# git (har har) repos
git clone https://github.com/jcanny/BIDMat.git && git clone git@github.com:shaddi/bdm-project2.git 

# copy bidmat stuff into project directory
cp -pr BIDMAT/* bdm-project2/.

# get data
wget --http-user=cs294_1 --http-password=sp2013mn http://www.cs.berkeley.edu/~jfc/DataMining/SP13/restricted/HW2/books/tokens.bin.gz
gunzip tokens.bin.gz
mv tokens.bin bdm-project2/.

echo "Project 2 ready to run."
