#!/bin/bash

DIR=`pwd`

echo Installing vim config...
ln -fs $DIR/vimrc $HOME/.vimrc
ln -fs $DIR/vim $HOME/.vim
mkdir -p /var/tmp/vim-kris/swap
mkdir -p /var/tmp/vim-kris/undo

echo Installing zsh config...
ln -fs $DIR/zshcomplete $HOME/.zshcomplete
ln -fs $DIR/zshrc $HOME/.zshrc
ln -fs $DIR/zshenv $HOME/.zshenv
echo Setting default shell to zsh...
chsh -s `which zsh`

echo Installing git config...
ln -fs $DIR/gitconfig $HOME/.gitconfig

echo Installing screen config...
ln -fs $DIR/screenrc $HOME/.screenrc

echo Installing irssi config...
ln -fs $DIR/irssi $HOME/.irssi

echo Installing xmonad config...
ln -fs $DIR/xmonad $HOME/.xmonad
ln -fs $DIR/xmobarrc $HOME/.xmobarrc

echo Installing xbindkeys config...
ln -fs $DIR/xbindkeysrc $HOME/.xbindkeysrc

echo All done!

