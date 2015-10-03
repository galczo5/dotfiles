#!/bin/bash

echo "---VIM Installer---"
echo "---galczo5/MyVimConfiguration"
echo "---version 0.1

"

echo "Installation of vim, vim-gtk and git"
#Install vim and git
sudo apt-get install vim vim-gtk git

echo "Cloning Vundle"
#Get Vundle and place in ~/.vim/
git clone https://github.com/gmarik/Vundle.vim.git $HOME/.vim/bundle/Vundle.vim

echo "Cloning .vimrc"
#Get .vimrc from my github and place it in ~/
#git clone https://github.com/galczo5/MyVimConfiguration temp 
cp .vimrc $HOME/.vimrc

echo "Installing plugins"
cd $HOME
vim +PluginInstall +qall

