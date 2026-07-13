#!/bin/bash

# Apply personal macOS system preferences via `defaults write`.
# Safe to re-run.

set -e

echo "Keyboard: fast key repeat rate + short delay until repeat"
defaults write NSGlobalDomain KeyRepeat -int 2
defaults write NSGlobalDomain InitialKeyRepeat -int 15

echo "Keyboard: disable press-and-hold accent picker (favor key repeat)"
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

echo "Finder: show hidden files"
defaults write com.apple.finder AppleShowAllFiles -bool true

echo "Dock: disable autohide, faster autohide animation for when it's toggled on"
defaults write com.apple.dock autohide -bool false
defaults write com.apple.dock autohide-time-modifier -float 0.15

# Hot corner action codes: 0 disabled, 2 Mission Control, 3 application windows,
# 4 desktop, 5 start screen saver, 6 disable screen saver, 10 sleep display,
# 11 Launchpad, 12 notification center, 13 lock screen, 14 quick note.
echo "Hot corners: top-left Mission Control, others disabled"
defaults write com.apple.dock wvous-tl-corner -int 2
defaults write com.apple.dock wvous-tl-modifier -int 0
defaults write com.apple.dock wvous-tr-corner -int 0
defaults write com.apple.dock wvous-tr-modifier -int 0
defaults write com.apple.dock wvous-bl-corner -int 0
defaults write com.apple.dock wvous-bl-modifier -int 0
defaults write com.apple.dock wvous-br-corner -int 0
defaults write com.apple.dock wvous-br-modifier -int 0

echo "Restarting affected apps..."
killall Finder >/dev/null 2>&1 || true
killall Dock >/dev/null 2>&1 || true

echo "Done. Some changes may require logging out or restarting."

echo ""
echo "The following are not scriptable and must be set by hand in"
echo "System Settings > Siri & Spotlight:"
echo "  - Results from Apps: disable all of them (app content isn't the"
echo "                        Applications category, so none should stay on)"
echo "  - Results from System: disable Files, Folders, iPhone Apps, Menu Items"
echo "                          (leave Apps enabled)"
read -r -p "Press enter once you've finished the manual Spotlight configuration... "
