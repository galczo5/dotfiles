-- windowsnap.lua
-- Drag-to-edge window snapping with uniform gaps.
-- Disable macOS native tiling first:
--   System Settings → Desktop & Dock → uncheck "Tile by dragging windows to screen edges"

local GAP       = 8   -- gap from screen edges and between adjacent snapped windows (px)
local EDGE_SIZE = 30  -- depth of edge trigger zones (px)
local DRAG_MIN  = 5   -- minimum drag distance before snap zones activate (px)
local TITLEBAR  = 28  -- approximate title bar height used to detect window drags (px)

-- ── drag state (declared early so wall helpers close over dragWindow) ─────────

local snapTarget = nil
local dragStart  = nil
local isDragWin  = false
local dragWindow = nil   -- window being dragged; excluded from wall scans
local wallFrames = {}    -- snapshot of other windows' frames, taken once per drag

-- ── wall detection ────────────────────────────────────────────────────────────
-- Walls are computed from wallFrames (a snapshot captured at mouseDown) rather
-- than querying hs.window.visibleWindows() on every drag event, which is slow.

local function onScreen(wf, sf)
    local cx, cy = wf.x + wf.w / 2, wf.y + wf.h / 2
    return cx >= sf.x and cx < sf.x + sf.w and cy >= sf.y and cy < sf.y + sf.h
end

-- Nearest left-edge among windows centered in the right half → right wall.
local function rightWall(scr, sf)
    local wall = sf.x + sf.w - GAP
    local mid  = sf.x + sf.w / 2
    for _, wf in ipairs(wallFrames) do
        if onScreen(wf, sf) and wf.x + wf.w / 2 > mid then wall = math.min(wall, wf.x) end
    end
    return wall
end

-- Nearest right-edge among windows centered in the left half → left wall.
local function leftWall(scr, sf)
    local wall = sf.x + GAP
    local mid  = sf.x + sf.w / 2
    for _, wf in ipairs(wallFrames) do
        if onScreen(wf, sf) and wf.x + wf.w / 2 < mid then wall = math.max(wall, wf.x + wf.w) end
    end
    return wall
end

local function leftSnap(scr, sf, y, h)
    local g  = GAP
    local rw = rightWall(scr, sf)
    local w  = (rw >= sf.x + sf.w - g)
               and (sf.w / 2 - g * 1.5)
               or  (rw - sf.x - g - g)
    return { x = sf.x + g, y = y, w = w, h = h }
end

local function rightSnap(scr, sf, y, h)
    local g      = GAP
    local lw     = leftWall(scr, sf)
    local noWall = lw <= sf.x + g
    local x = noWall and (sf.x + sf.w / 2 + g / 2) or (lw + g)
    local w = noWall and (sf.w / 2 - g * 1.5)      or (sf.x + sf.w - g - x)
    return { x = x, y = y, w = w, h = h }
end

-- ── snap geometry ─────────────────────────────────────────────────────────────

local function screenForPoint(pt)
    for _, scr in ipairs(hs.screen.allScreens()) do
        local f = scr:frame()
        if pt.x >= f.x and pt.x < f.x+f.w and pt.y >= f.y and pt.y < f.y+f.h then
            return scr
        end
    end
    return hs.screen.mainScreen()
end

local function computeTarget(pt)
    local scr = screenForPoint(pt)
    local f   = scr:frame()
    local rx  = pt.x - f.x
    local ry  = pt.y - f.y
    local W, H, g = f.w, f.h, GAP

    if ry < EDGE_SIZE then
        local t = rx / W
        if     t < 1/3 then return { x=f.x+g,         y=f.y+g, w=W*2/3-g*1.5, h=H-g*2 }
        elseif t > 2/3 then return { x=f.x+W*2/3+g/2, y=f.y+g, w=W/3-g*1.5,   h=H-g*2 }
        else                 return { x=f.x+g,         y=f.y+g, w=W-g*2,        h=H-g*2 } end
    end

    -- left edge: corner | adaptive width | fixed 50% | corner (top → bottom)
    if rx < EDGE_SIZE then
        local t = ry / H
        if     t < 1/4 then return leftSnap(scr, f, f.y+g,       H/2-g*1.5)            -- top corner
        elseif t < 1/2 then return leftSnap(scr, f, f.y+g,       H-g*2)                -- adaptive, full height
        elseif t < 3/4 then return { x=f.x+g, y=f.y+g, w=W/2-g*1.5, h=H-g*2 }          -- fixed 50%, full height
        else                 return leftSnap(scr, f, f.y+H/2+g/2, H/2-g*1.5)           -- bottom corner
        end
    end

    -- right edge: corner | adaptive width | fixed 50% | corner (top → bottom)
    if rx > W - EDGE_SIZE then
        local t = ry / H
        if     t < 1/4 then return rightSnap(scr, f, f.y+g,       H/2-g*1.5)           -- top corner
        elseif t < 1/2 then return rightSnap(scr, f, f.y+g,       H-g*2)               -- adaptive, full height
        elseif t < 3/4 then return { x=f.x+W/2+g/2, y=f.y+g, w=W/2-g*1.5, h=H-g*2 }    -- fixed 50%, full height
        else                 return rightSnap(scr, f, f.y+H/2+g/2, H/2-g*1.5)          -- bottom corner
        end
    end

    return nil
end

-- ── preview overlay ───────────────────────────────────────────────────────────

local snapOverlay = nil   -- persistent canvas, created once and reused
local shownFrame  = nil   -- geometry currently drawn, to skip redundant redraws

local function sameFrame(a, b)
    return a and b and a.x==b.x and a.y==b.y and a.w==b.w and a.h==b.h
end

local function showOverlay(t)
    if sameFrame(shownFrame, t) then return end   -- nothing changed; avoid redraw
    shownFrame = { x=t.x, y=t.y, w=t.w, h=t.h }

    if not snapOverlay then
        snapOverlay = hs.canvas.new({ x=t.x, y=t.y, w=t.w, h=t.h })
        snapOverlay:level(hs.canvas.windowLevels.overlay - 1)
    else
        snapOverlay:frame({ x=t.x, y=t.y, w=t.w, h=t.h })
    end

    snapOverlay:replaceElements(
        {
            type             = "rectangle",
            action           = "fill",
            fillColor        = { white=1, alpha=0.25 },
            roundedRectRadii = { xRadius=10, yRadius=10 },
            frame            = { x=0, y=0, w=t.w, h=t.h },
        },
        {
            type             = "rectangle",
            action           = "stroke",
            strokeColor      = { white=1, alpha=0.95 },
            strokeWidth      = 4,
            roundedRectRadii = { xRadius=10, yRadius=10 },
            frame            = { x=2, y=2, w=t.w-4, h=t.h-4 },
        }
    )
    snapOverlay:show()
end

local function hideOverlay()
    shownFrame = nil
    if snapOverlay then snapOverlay:hide() end
end

-- ── drag tracking ─────────────────────────────────────────────────────────────

-- Scan all visible windows for one whose title bar contains pt.
-- More reliable than focusedWindow() at mouseDown time because clicking a
-- background window hasn't changed focus yet when the event fires.
local function windowAtTitleBar(pt)
    for _, w in ipairs(hs.window.visibleWindows()) do
        if w:isStandard() then
            local wf = w:frame()
            if pt.x >= wf.x and pt.x <= wf.x + wf.w
            and pt.y >= wf.y and pt.y <= wf.y + TITLEBAR then
                return w
            end
        end
    end
    return nil
end

local downWatcher = hs.eventtap.new({ hs.eventtap.event.types.leftMouseDown }, function(e)
    local pt = e:location()
    dragWindow = windowAtTitleBar(pt)
    isDragWin  = dragWindow ~= nil
    dragStart  = pt
    snapTarget = nil

    -- Snapshot other windows' frames once, so drag events don't repeatedly
    -- hit the (slow) Accessibility API while computing snap walls.
    wallFrames = {}
    if isDragWin then
        for _, w in ipairs(hs.window.visibleWindows()) do
            if w ~= dragWindow and w:isStandard() then
                wallFrames[#wallFrames + 1] = w:frame()
            end
        end
    end
    return false
end)

local dragWatcher = hs.eventtap.new({ hs.eventtap.event.types.leftMouseDragged }, function(e)
    if not dragStart or not isDragWin then return false end
    local pt = e:location()
    local dx = pt.x - dragStart.x
    local dy = pt.y - dragStart.y
    if dx*dx + dy*dy < DRAG_MIN*DRAG_MIN then return false end

    local target = computeTarget(pt)
    if target then
        snapTarget = target
        showOverlay(target)
    else
        snapTarget = nil
        hideOverlay()
    end
    return false
end)

local upWatcher = hs.eventtap.new({ hs.eventtap.event.types.leftMouseUp }, function(e)
    local target = snapTarget
    local win    = dragWindow
    snapTarget = nil
    dragStart  = nil
    isDragWin  = false
    dragWindow = nil
    wallFrames = {}
    hideOverlay()

    if target and win then
        -- Small delay lets macOS finish processing mouse-up before we resize.
        hs.timer.doAfter(0.05, function()
            win:setFrame(hs.geometry.new(target), 0)
        end)
    end
    return false
end)

downWatcher:start()
dragWatcher:start()
upWatcher:start()

return { downWatcher=downWatcher, dragWatcher=dragWatcher, upWatcher=upWatcher }
