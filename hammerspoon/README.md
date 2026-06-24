# Hammerspoon config

[Hammerspoon](https://www.hammerspoon.org/) automation for macOS. The config is
split into a small entry point plus two self-contained feature modules.

```
init.lua            -- entry point: hotkeys + requires the modules
windowswitcher.lua  -- alt-tab style hold-and-cycle window switcher
windowsnap.lua      -- drag-to-edge window snapping with gaps
```

Reload after editing via the Hammerspoon menubar icon → **Reload Config**
(or `⌘R` in the Hammerspoon console).

---

## init.lua

The entry point. It:

- Binds **`⌘ + Return`** to launch or focus **Ghostty**.
- `require`s the two feature modules so they load on start.

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

---

## windowsnap.lua

Custom drag-to-edge window snapping with uniform gaps, replacing the macOS
native edge tiling.

> **Setup:** disable the built-in feature first —
> **System Settings → Desktop & Dock → uncheck "Tile by dragging windows to
> screen edges"** — otherwise the two systems fight over the drag.

**Usage**

Drag a window by its title bar so the cursor reaches a screen edge. A white
preview overlay shows where the window will land; release the mouse to snap it
there. Every snap leaves a `GAP` (8px) margin from screen edges and between
adjacent windows.

**Zones**

Top edge — three horizontal zones:

```
┌──────┬──────────────┬──────┐
│ 2/3  │  fullscreen  │ 1/3  │
└──────┴──────────────┴──────┘
```

Left and right edges — four vertical zones (top → bottom):

```
┌──────────┐
│ corner   │  ¼ height → top corner
├──────────┤
│ adaptive │  full height, fills to the next window (or 50%)
├──────────┤
│  50%     │  full height, exactly half width
├──────────┤
│ corner   │  ¼ height → bottom corner
└──────────┘
```

**Adaptive width.** The adaptive zone (and the corners) size themselves to the
free space: the module finds the nearest window *centered* on the opposite half
of the screen and fills up to it. If that side is empty it falls back to 50%.
So if a 2/3-width window already occupies the right, snapping to the left edge
gives the remaining 1/3.

**Implementation notes**

- Three eventtaps track the gesture: `leftMouseDown` (identify the dragged
  window via title-bar hit test + snapshot other windows' frames),
  `leftMouseDragged` (compute the target zone and draw the overlay), and
  `leftMouseUp` (apply the snap).
- **Performance:** the dragged window is found by scanning all visible windows
  rather than `focusedWindow()` (which still points at the previous window when
  the click lands). Other windows' frames are snapshotted once at mouse-down so
  the high-frequency drag events never hit the slow Accessibility API. The
  overlay canvas is created once and reused, and only redraws when the target
  zone actually changes.

**Tunables** (top of file): `GAP` (edge/inter-window gap), `EDGE_SIZE` (depth of
the trigger zones), `DRAG_MIN` (min drag distance before zones activate),
`TITLEBAR` (assumed title-bar height for drag detection).
