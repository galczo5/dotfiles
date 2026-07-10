# CLAUDE.md

Personal macOS dotfiles. Configs are deployed as **symlinks** from this repo into `~/.config` and friends, so edits here take effect immediately. Installation is driven by four interactive scripts that all share one picker.

## How it works

Each script sources `script/pick.sh` and calls `pick` with a list of `"Display name|shell command"` entries. `pick` renders an interactive menu (`↑`/`↓` move, `space` toggle, `a` all, `n` none, `enter` run, `q` quit) and `eval`s the command of every selected entry in the current shell. Functions defined in the calling script (e.g. `link`, `unlink_file`) are available to those commands.

| Script | Purpose | Entry command runs |
|--------|---------|--------------------|
| `brew-install.sh` | Install apps/CLI tools | `brew install …` |
| `brew-uninstall.sh` | Remove apps/CLI tools | `brew uninstall …` |
| `install.sh` | Symlink configs into place | `link <repo path> <dest>` |
| `uninstall.sh` | Remove those symlinks | `unlink_file <dest>` |

`install.sh` prompts before replacing an existing destination and is safe to re-run. `$DOTFILES_DIR` is the repo root; use it for absolute paths.

## Adding a package (Homebrew)

Add one entry to **both** `brew-install.sh` and `brew-uninstall.sh`, keeping them in the same order.

```sh
# brew-install.sh
"ripgrep|brew install ripgrep" \

# brew-uninstall.sh
"ripgrep|brew uninstall ripgrep" \
```

For GUI apps use a cask in both files:

```sh
"raycast|brew install --cask raycast" \      # brew-install.sh
"raycast|brew uninstall --cask raycast" \    # brew-uninstall.sh
```

## Adding a config (symlink)

1. Add the config file(s) under a directory in the repo (e.g. `mytool/config`).
2. Add a matching entry to **both** `install.sh` and `uninstall.sh`.

```sh
# install.sh — link <repo-relative source> <destination>
"mytool|link mytool/config $HOME/.config/mytool/config" \

# uninstall.sh — unlink_file <destination>
"mytool|unlink_file $HOME/.config/mytool/config" \
```

`link` takes a path **relative to the repo root** as its first argument and an absolute destination as its second; it creates parent directories automatically. `unlink_file` takes the same absolute destination and only removes it if it exists (safely handling both symlinks and plain files). Keep the two files in the same order.

## Adding a one-off action

Entries don't have to be `link`/`brew` — any shell command works. Non-config actions in `install.sh` call a helper script or command directly:

```sh
"hushlogin|touch $HOME/.hushlogin" \
"set zed as default editor|$DOTFILES_DIR/script/set-zed-default.sh" \
```

Put reusable helpers in `script/` and reference them via `$DOTFILES_DIR/script/…`.

## Conventions

- Every install entry should have a matching uninstall entry, in the same position/order.
- Use `$HOME`, not `~`, in entry commands (they're `eval`'d as strings).
- Use `$DOTFILES_DIR` for absolute repo paths; use repo-relative paths as the first arg to `link`.
- Update `README.md` (the Layout table / Highlights) when adding a new tool or notable feature.
