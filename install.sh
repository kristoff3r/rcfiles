#!/bin/bash

DIR=`pwd`
NAME="Kristoffer Søholm"
GITHUB_USER="kristoff3r"
LINUX_USER="kris"


echo Installing vim config...
mkdir -p /var/tmp/vim/swap
mkdir -p /var/tmp/vim/undo

echo Setting default shell to zsh...
#chsh -s `which zsh`

cd dot
for file in *; do ln -fs $DIR/dot/$file $HOME/.$file; done
cd $DIR

echo All done!
