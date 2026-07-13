# dotfiles

Personal macOS configuration files and setup scripts.

Everything is installed via symlinks, so edits in this repo take effect immediately. An interactive picker lets you choose exactly which configs and packages to install.

## Layout

| Path | What it configures |
|------|--------------------|
| `fish/` | Fish shell config, prompt, and a `fish_source` helper for sourcing POSIX files |
| `zshrc/` | `.zshrc` (Zsh environment; also sourced by Fish) |
| `vi/` | Minimal `.exrc` for `vi`/`ex` |
| `nvim/` | Neovim (`init.lua`) |
| `ghostty/` | Ghostty terminal |
| `alacritty/` | Alacritty terminal |
| `kitty/` | Kitty terminal |
| `lazygit/` | Lazygit |
| `mc/` | Midnight Commander (`ini`, `panels.ini`) |
| `zed/` | Zed editor (`settings.json`, `keymap.json`, `tasks.json`) |
| `hammerspoon/` | Hammerspoon — launch-or-focus hotkey, a custom window switcher, and drag-to-edge window snapping (see `hammerspoon/README.md`) |
| `script/` | Helper scripts (interactive picker, default-app setters, macOS defaults) |
| `brew-install.sh` | Installs apps and CLI tools via Homebrew |
| `install.sh` | Symlinks configs into place |
| `clean.sh` | Removes the symlinks created by `install.sh` |

## Usage

Clone the repo, then run the scripts. Each presents an interactive menu — use `↑`/`↓` to move, `space` to toggle, `a` for all, `n` for none, `enter` to run, `q` to quit.

```sh
git clone <repo-url> ~/Dev/dotfiles
cd ~/Dev/dotfiles

./brew-install.sh   # install packages (Homebrew required)
./install.sh        # symlink configs into ~/.config and friends
```

`install.sh` prompts before replacing anything that already exists at a destination, so it's safe to re-run.

### Removing

```sh
./clean.sh          # remove all symlinks this repo installed
```

## Highlights

- **Hammerspoon window switcher** (`hammerspoon/windowswitcher.lua`) — hold `alt` and tap `tab` to cycle through visible windows (`alt`+`shift`+`tab` to go back); release `alt` to focus. Hovering a row briefly previews that window. `cmd`+`return` launches or focuses Ghostty.
- **Hammerspoon window snapping** (`hammerspoon/windowsnap.lua`) — drag a window by its title bar to a screen edge to snap it, with uniform gaps. The top edge gives 2/3, full, and 1/3 zones; the left/right edges give four zones (corner, adaptive-width, fixed 50%, corner). The adaptive zone fills the free space up to the nearest window. Requires disabling macOS native edge tiling — see `hammerspoon/README.md`.
- **Default-app setters** — `install.sh` can optionally run `script/set-zed-default.sh` (Zed for text/source files) and `script/set-vlc-default.sh` (VLC for audio/video). Both use [`duti`](https://github.com/moretension/duti) (`brew install duti`).
- **macOS system defaults** — `script/macos-defaults.sh` sets a fast key repeat rate/delay, disables press-and-hold accent picker, shows hidden files in Finder, disables Dock autohide, and sets the top-left hot corner to Mission Control (others disabled). Run via `install.sh`, safe to re-run.
- **Shared shell environment** — Fish sources `~/.zprofile` and `~/.zshrc` via the `fish_source` function, keeping environment variables consistent across shells.

## Requirements

- macOS
- [Homebrew](https://brew.sh) (for `brew-install.sh`)
- `duti` is needed only for the default-app setters
