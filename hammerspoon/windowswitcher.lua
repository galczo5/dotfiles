-- Custom hold-and-cycle window switcher.
-- Hold alt, tap tab to cycle forward (alt+shift+tab = back), release alt to focus.
-- Each row: small app icon on the left, window title on the right.

local ROW_H    = 40
local ICON_SZ  = 22
local PAD      = 12
local WIDTH    = 420
local MAX_ROWS = 12

local windows      = {}
local selectedIdx  = 1
local overlay      = nil
local altIsDown    = false
local iconCache    = {}

local function icon(app)
    if not app then return nil end
    local id = app:bundleID()
    if not id then return nil end
    if not iconCache[id] then
        iconCache[id] = hs.image.imageFromAppBundle(id)
    end
    return iconCache[id]
end

local function visibleWindows()
    local wins = {}
    for _, w in ipairs(hs.window.orderedWindows()) do
        if w:isStandard() and w:isVisible() then
            wins[#wins + 1] = w
        end
    end
    return wins
end

local function hideOverlay()
    if overlay then
        overlay:delete()
        overlay = nil
    end
end

local function focusSelected()
    local win = windows[selectedIdx]
    hideOverlay()
    if win then win:focus() end
end

local function redraw()
    local count  = math.min(#windows, MAX_ROWS)
    local height = count * ROW_H + PAD * 2
    local sf     = hs.screen.mainScreen():frame()
    local ox     = math.floor((sf.w - WIDTH) / 2) + sf.x
    local oy     = math.floor((sf.h - height) / 2) + sf.y

    if overlay then overlay:delete() end
    overlay = hs.canvas.new({ x = ox, y = oy, w = WIDTH, h = height })
    overlay:level(hs.canvas.windowLevels.overlay)
    overlay:alpha(1)

    local els = {}

    -- background
    els[#els + 1] = {
        type             = "rectangle",
        action           = "fill",
        fillColor        = { red = 0.12, green = 0.12, blue = 0.12, alpha = 0.96 },
        roundedRectRadii = { xRadius = 12, yRadius = 12 },
        frame            = { x = 0, y = 0, w = WIDTH, h = height },
    }

    for i, win in ipairs(windows) do
        if i > MAX_ROWS then break end
        local ry  = PAD + (i - 1) * ROW_H
        local sel = i == selectedIdx

        -- row highlight
        if sel then
            els[#els + 1] = {
                type             = "rectangle",
                action           = "fill",
                fillColor        = { red = 0.25, green = 0.45, blue = 0.85, alpha = 0.75 },
                roundedRectRadii = { xRadius = 7, yRadius = 7 },
                frame            = { x = PAD / 2, y = ry + 2, w = WIDTH - PAD, h = ROW_H - 4 },
            }
        end

        -- app icon
        local img = icon(win:application())
        if img then
            els[#els + 1] = {
                type  = "image",
                image = img,
                frame = {
                    x = PAD,
                    y = ry + (ROW_H - ICON_SZ) / 2,
                    w = ICON_SZ,
                    h = ICON_SZ,
                },
                imageScaling = "scaleProportionally",
            }
        end

        -- window title
        local app   = win:application()
        local title = win:title()
        if not title or title == "" then
            title = app and app:name() or "Untitled"
        end
        els[#els + 1] = {
            type      = "text",
            text      = hs.styledtext.new(title, {
                font       = { name = "System Font Regular", size = 13 },
                color      = { white = 1, alpha = sel and 1.0 or 0.75 },
                paragraphStyle = { lineBreak = "truncatingTail" },
            }),
            frame = {
                x = PAD + ICON_SZ + 10,
                y = ry + (ROW_H - 18) / 2,
                w = WIDTH - PAD * 2 - ICON_SZ - 10,
                h = 20,
            },
        }
    end

    overlay:replaceElements(els)
    overlay:show()
end

local function showOrCycle(dir)
    altIsDown = true
    if not overlay then
        windows     = visibleWindows()
        selectedIdx = math.min(2, #windows)
        if #windows > 0 then redraw() end
    else
        selectedIdx = ((selectedIdx - 1 + dir) % #windows) + 1
        redraw()
    end
end

-- Detect alt release to commit selection.
-- Check the specific keycode (58 = left alt, 61 = right alt) rather than
-- relying on the altIsDown flag, which can be missed if flagsChanged fires
-- before the hotkey sets it.
local flagWatcher = hs.eventtap.new({ hs.eventtap.event.types.flagsChanged }, function(evt)
    local kc = evt:getKeyCode()
    if (kc == 58 or kc == 61) and not evt:getFlags().alt then
        altIsDown = false
        if overlay then focusSelected() end
    end
end)
flagWatcher:start()

hs.hotkey.bind({ "alt" },         "tab", function() showOrCycle(1)  end)
hs.hotkey.bind({ "alt", "shift"}, "tab", function() showOrCycle(-1) end)

-- Return the watcher so it isn't garbage collected.
return { flagWatcher = flagWatcher }
