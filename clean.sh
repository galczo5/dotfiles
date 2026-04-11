#!/bin/bash

DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"

unlink_file() {
    local dst="$1"

    if [ -L "$dst" ]; then
        rm "$dst"
        echo "[REMOVED] $dst (symlink)"
    elif [ -e "$dst" ]; then
        rm "$dst"
        echo "[REMOVED] $dst (file)"
    else
        echo "[SKIP] $dst does not exist"
    fi
}

# Fish
unlink_file "$HOME/.config/fish/config.fish"

# Ghostty
unlink_file "$HOME/.config/ghostty/config"

# Alacritty
unlink_file "$HOME/.config/alacritty/alacritty.toml"

# Kitty
unlink_file "$HOME/.config/kitty/kitty.conf"

# Lazygit
unlink_file "$HOME/.config/lazygit/config.yml"

# Midnight Commander
unlink_file "$HOME/.config/mc/ini"
unlink_file "$HOME/.config/mc/panels.ini"

# Zed
unlink_file "$HOME/.config/zed/settings.json"
unlink_file "$HOME/.config/zed/keymap.json"
unlink_file "$HOME/.config/zed/tasks.json"

# Neovim
unlink_file "$HOME/.config/nvim/init.lua"

# Vi
unlink_file "$HOME/.exrc"

# Zsh
unlink_file "$HOME/.zshrc"
