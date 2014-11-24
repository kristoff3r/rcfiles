#!/bin/sh

DIR=`pwd`
NAME="Kristoffer Søholm"
GITHUB_USER="kristoff3r"
LINUX_USER="kris"

echo "Making vim swap/undo directories..."
sudo mkdir -p /var/tmp/vim/swap
sudo mkdir -p /var/tmp/vim/undo
sudo chown -R $LINUX_USER: /var/tmp/vim

cd $DIR/dot
for file in *; do 
    ln -fs $DIR/dot/$file $HOME/.$file;
    echo "Created symlink for .$file";
done
cd $DIR

echo "All done!"
