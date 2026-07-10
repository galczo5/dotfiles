#!/bin/bash

DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$DOTFILES_DIR/script/pick.sh"

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

pick \
    "fish config|unlink_file $HOME/.config/fish/config.fish" \
    "fish source function|unlink_file $HOME/.config/fish/functions/fish_source.fish" \
    "ghostty|unlink_file $HOME/.config/ghostty/config" \
    "alacritty|unlink_file $HOME/.config/alacritty/alacritty.toml" \
    "kitty|unlink_file $HOME/.config/kitty/kitty.conf" \
    "lazygit|unlink_file $HOME/.config/lazygit/config.yml" \
    "mc ini|unlink_file $HOME/.config/mc/ini" \
    "mc panels|unlink_file $HOME/.config/mc/panels.ini" \
    "hammerspoon|unlink_file $HOME/.hammerspoon/init.lua" \
    "hammerspoon windowswitcher|unlink_file $HOME/.hammerspoon/windowswitcher.lua" \
    "hammerspoon windowsnap|unlink_file $HOME/.hammerspoon/windowsnap.lua" \
    "zed settings|unlink_file $HOME/.config/zed/settings.json" \
    "zed keymap|unlink_file $HOME/.config/zed/keymap.json" \
    "zed tasks|unlink_file $HOME/.config/zed/tasks.json" \
    "neovim|unlink_file $HOME/.config/nvim/init.lua" \
    "vi exrc|unlink_file $HOME/.exrc" \
    "zshrc|unlink_file $HOME/.zshrc" \
    "hushlogin|unlink_file $HOME/.hushlogin"
