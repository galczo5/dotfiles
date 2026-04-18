#!/bin/bash

confirm() {
    read -r -p "Install $1? [y/N] " reply
    [[ "$reply" =~ ^[Yy]$ ]]
}

confirm "fish" && brew install fish
confirm "neovim" && brew install neovim
confirm "lazygit" && brew install lazygit
confirm "midnight-commander" && brew install midnight-commander
confirm "htop" && brew install htop
confirm "fnm" && brew install fnm
confirm "zed" && brew install zed
confirm "ghostty" && brew install --cask ghostty
confirm "obsidian" && brew install --cask obsidian
confirm "skhd" && brew install --HEAD asmvik/skhd/skhd
confirm "alt-tab" && brew install --cask alt-tab
confirm "marta" && brew install --cask marta
