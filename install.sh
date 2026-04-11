#!/bin/bash

DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"

link() {
    local src="$DOTFILES_DIR/$1"
    local dst="$2"

    if [ -e "$dst" ] || [ -L "$dst" ]; then
        echo "[SKIP] $dst already exists"
        return
    fi

    mkdir -p "$(dirname "$dst")"
    ln -s "$src" "$dst"
    echo "[LINK] $src -> $dst"
}

# Fish
link "fish/config.fish" "$HOME/.config/fish/config.fish"

# Ghostty
link "ghostty/config" "$HOME/.config/ghostty/config"

# Alacritty
link "alacritty/alacritty.toml" "$HOME/.config/alacritty/alacritty.toml"

# Kitty
link "kitty/kitty.conf" "$HOME/.config/kitty/kitty.conf"

# Lazygit
link "lazygit/config.yml" "$HOME/.config/lazygit/config.yml"

# Midnight Commander
link "mc/ini" "$HOME/.config/mc/ini"
link "mc/panels.ini" "$HOME/.config/mc/panels.ini"

# Zed
link "zed/settings.json" "$HOME/.config/zed/settings.json"
link "zed/keymap.json" "$HOME/.config/zed/keymap.json"
link "zed/tasks.json" "$HOME/.config/zed/tasks.json"

# Neovim
link "nvim/init.lua" "$HOME/.config/nvim/init.lua"

# Vi
link "vi/.exrc" "$HOME/.exrc"

# Zsh
link "zshrc/.zshrc" "$HOME/.zshrc"

# Hushlogin
touch "$HOME/.hushlogin"
