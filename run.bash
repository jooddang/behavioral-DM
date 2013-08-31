#!/bin/bash

# get bidmat
git clone https://github.com/jcanny/BIDMat.git
cp -pr BIDMat/* .

# get the tokens.bin
wget --http-user=cs294_1 --http-password=sp2013mn http://www.cs.berkeley.edu/~jfc/DataMining/SP13/restricted/HW2/books/tokens.bin.gz
gunzip tokens.bin.gz

# get the dict.txt
wget http://www.cs.berkeley.edu/~shaddi/bdm/dict.txt.gz
gunzip dict.txt.gz

# get the data folder
wget http://www.cs.berkeley.edu/~shaddi/bdm/data10.tar.gz
mv data10.tar.gz data.tar.gz
tar xzvf data.tar.gz
rm data.tar.gz

echo "Ready to run. Start ./bidmat, then enter :load p2.scala."
