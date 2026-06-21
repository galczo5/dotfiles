#!/bin/bash

DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"
source "$DOTFILES_DIR/script/pick.sh"

link() {
    local src="$DOTFILES_DIR/$1"
    local dst="$2"

    if [ -e "$dst" ] || [ -L "$dst" ]; then
        read -r -p "[EXISTS] $dst — remove and replace? [y/N] " reply
        if [[ "$reply" =~ ^[Yy]$ ]]; then
            rm -rf "$dst"
        else
            echo "[SKIP] $dst"
            return
        fi
    fi

    mkdir -p "$(dirname "$dst")"
    ln -s "$src" "$dst"
    echo "[LINK] $src -> $dst"
}

pick \
    "fish config|link fish/config.fish $HOME/.config/fish/config.fish" \
    "fish source function|link fish/functions/fish_source.fish $HOME/.config/fish/functions/fish_source.fish" \
    "ghostty|link ghostty/config $HOME/.config/ghostty/config" \
    "alacritty|link alacritty/alacritty.toml $HOME/.config/alacritty/alacritty.toml" \
    "kitty|link kitty/kitty.conf $HOME/.config/kitty/kitty.conf" \
    "lazygit|link lazygit/config.yml $HOME/.config/lazygit/config.yml" \
    "mc ini|link mc/ini $HOME/.config/mc/ini" \
    "mc panels|link mc/panels.ini $HOME/.config/mc/panels.ini" \
    "hammerspoon|link hammerspoon/init.lua $HOME/.hammerspoon/init.lua" \
    "hammerspoon windowswitcher|link hammerspoon/windowswitcher.lua $HOME/.hammerspoon/windowswitcher.lua" \
    "hammerspoon windowsnap|link hammerspoon/windowsnap.lua $HOME/.hammerspoon/windowsnap.lua" \
    "zed settings|link zed/settings.json $HOME/.config/zed/settings.json" \
    "zed keymap|link zed/keymap.json $HOME/.config/zed/keymap.json" \
    "zed tasks|link zed/tasks.json $HOME/.config/zed/tasks.json" \
    "neovim|link nvim/init.lua $HOME/.config/nvim/init.lua" \
    "vi exrc|link vi/.exrc $HOME/.exrc" \
    "zshrc|link zshrc/.zshrc $HOME/.zshrc" \
    "hushlogin|touch $HOME/.hushlogin" \
    "set zed as default editor|$DOTFILES_DIR/script/set-zed-default.sh" \
    "set vlc as default media player|$DOTFILES_DIR/script/set-vlc-default.sh"
