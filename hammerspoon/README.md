# Hammerspoon config

[Hammerspoon](https://www.hammerspoon.org/) automation for macOS. The config is
split into a small entry point plus a self-contained feature module.

```
init.lua            -- entry point: hotkeys + requires the module
windowswitcher.lua  -- alt-tab style hold-and-cycle window switcher
```

Reload after editing via the Hammerspoon menubar icon → **Reload Config**
(or `⌘R` in the Hammerspoon console).

---

## init.lua

The entry point. It:

- Binds **`⌘ + Return`** to launch or focus **Ghostty**.
- `require`s the feature module so it loads on start.

---

## windowswitcher.lua

A custom hold-and-cycle window switcher (a nicer alt-tab).

**Usage**

- Hold **`alt`** and tap **`tab`** to cycle forward through windows.
- **`alt + shift + tab`** cycles backward.
- Release **`alt`** to focus the highlighted window.

**Behaviour**

- An overlay panel appears centered on the main screen, one row per window:
  small app icon on the left, window title on the right. The current selection
  is highlighted.
- Only **standard, visible** windows are listed, in macOS window order.
- **Live preview:** after hovering a selection for `PREVIEW_DELAY` (0.25s) the
  window is raised behind the overlay so you can see it. If you move on or
  cancel without committing, the originally-frontmost window is raised back to
  restore the previous order.
- `alt` release is detected via a `flagsChanged` eventtap that checks the
  specific key codes (58 = left alt, 61 = right alt), which is more reliable
  than tracking a flag that the hotkey might set too late.

**Tunables** (top of file): `ROW_H`, `ICON_SZ`, `PAD`, `MAX_ROWS`, `MARGIN`,
`PREVIEW_DELAY`.
