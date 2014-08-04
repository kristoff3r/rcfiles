#!/bin/bash

DIR=`pwd`
NAME="Kristoffer Søholm"
GITHUB_USER="kristoff3r"
LINUX_USER="kris"

echo "Making vim swap/undo directories..."
#mkdir -p /var/tmp/vim/swap
#mkdir -p /var/tmp/vim/undo

cd $DIR/dot
for file in *; do 
#    --ln -fs $DIR/dot/$file $HOME/.$file;
    echo "Created symlink for .$file";
done
cd $DIR

echo All done!
