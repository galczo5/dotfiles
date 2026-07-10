#!/bin/bash

source "$(cd "$(dirname "$0")" && pwd)/script/pick.sh"

pick \
    "fish|brew uninstall fish" \
    "neovim|brew uninstall neovim" \
    "lazygit|brew uninstall lazygit" \
    "midnight-commander|brew uninstall midnight-commander" \
    "htop|brew uninstall htop" \
    "fnm|brew uninstall fnm" \
    "zed|brew uninstall zed" \
    "ghostty|brew uninstall --cask ghostty" \
    "obsidian|brew uninstall --cask obsidian" \
    "hammerspoon|brew uninstall --cask hammerspoon" \
    "marta|brew uninstall --cask marta" \
    "vlc|brew uninstall --cask vlc" \
    "duti|brew uninstall duti" \
    "spotify|brew uninstall --cask spotify" \
    "discord|brew uninstall --cask discord" \
    "kepubify|brew uninstall kepubify" \
    "fzf|brew uninstall fzf" \
    "github-cli|brew uninstall gh" \
    "aws-cli|brew uninstall awscli" \
    "terraform|brew uninstall terraform"
