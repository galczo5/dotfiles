#!/bin/bash

source "$(cd "$(dirname "$0")" && pwd)/script/pick.sh"

pick \
    "fish|brew install fish" \
    "neovim|brew install neovim" \
    "lazygit|brew install lazygit" \
    "midnight-commander|brew install midnight-commander" \
    "htop|brew install htop" \
    "fnm|brew install fnm" \
    "zed|brew install zed" \
    "ghostty|brew install --cask ghostty" \
    "obsidian|brew install --cask obsidian" \
    "hammerspoon|brew install --cask hammerspoon" \
    "alt-tab|brew install --cask alt-tab" \
    "marta|brew install --cask marta" \
    "vlc|brew install --cask vlc" \
    "duti|brew install duti" \
    "spotify|brew install --cask spotify" \
    "discord|brew install --cask discord" \
    "kepubify|brew install kepubify"
