#!/bin/bash

DIR=`pwd`

sudo apt-get install $(< packages)

echo "Installing zsh + oh-my-zsh..."
git clone git://github.com/robbyrussell/oh-my-zsh.git $HOME/.oh-my-zsh
chsh -s /bin/zsh

echo "Creating directories..."
mkdir -p $HOME/git

echo "Making vim swap/undo directories..."
mkdir -p $HOME/.vim/swap
mkdir -p $HOME/.vim/undo

echo "Copying dot files..."
cd $DIR/dot
for file in *; do 
    ln -fs $DIR/dot/$file $HOME/.$file;
    echo "Created symlink for .$file";
done
cd $DIR

ln -fs $DIR/bin $HOME/bin

echo "Install dropbox"
$HOME/bin/dropbox.py start -i
ln -fs $HOME/Dropbox/uni    $HOME/uni
ln -fs $HOME/Dropbox/pwnies $HOME/pwnies

echo All done!
