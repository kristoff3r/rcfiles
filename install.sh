#!/bin/bash

DIR=`pwd`

echo Installing vim config...
ln -Tfs $DIR/vimrc $HOME/.vimrc
ln -Tfs $DIR/vim/ $HOME/.vim
mkdir -p /var/tmp/vim-kris/swap
mkdir -p /var/tmp/vim-kris/undo

echo Installing zsh config...
ln -Tfs $DIR/zshcomplete $HOME/.zshcomplete
ln -Tfs $DIR/zshrc $HOME/.zshrc
ln -Tfs $DIR/zshenv $HOME/.zshenv
echo Setting default shell to zsh...
chsh -s `which zsh`

echo Installing git config...
ln -Tfs $DIR/gitconfig $HOME/.gitconfig

echo Installing screen config...
ln -Tfs $DIR/screenrc $HOME/.screenrc

echo Installing irssi config...
ln -Tfs $DIR/irssi $HOME/.irssi

echo Installing xmonad config...
ln -Tfs $DIR/xmonad/ $HOME/.xmonad
ln -Tfs $DIR/xmobarrc $HOME/.xmobarrc
ln -Tfs $DIR/xsession $HOME/.xsession
ln -Tfs $DIR/Xdefaults $HOME/.Xdefaults
mkdir -p $HOME/bin
cd $DIR/status
for i in *; do ln -Tfs $DIR/status/$i $HOME/bin/$i; done
cd $DIR

echo Installing gdb config...
ln -Tfs $DIR/gdbinit $HOME/.gdbinit

echo Installing xbindkeys config...
ln -Tfs $DIR/xbindkeysrc $HOME/.xbindkeysrc
ln -Tfs $DIR/xmodmap $HOME/.xmodmap


echo All done!

