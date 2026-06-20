-- Custom hold-and-cycle window switcher.
-- Hold alt, tap tab to cycle forward (alt+shift+tab = back), release alt to focus.
-- Each row: small app icon on the left, window title on the right.

local ROW_H    = 40
local ICON_SZ  = 22
local PAD      = 12
local MAX_ROWS = 12
local MARGIN   = 24  -- transparent border around the panel so the shadow isn't clipped

local PREVIEW_DELAY = 0.25  -- seconds to hover before previewing the window

local windows       = {}
local selectedIdx   = 1
local overlay       = nil
local altIsDown     = false
local iconCache     = {}
local previewTimer  = nil   -- fires after hovering, raises the selected window
local previewActive = false -- whether a window has been raised for preview
local originalFront = nil   -- frontmost window when the switcher opened

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

-- Cancel any pending/active preview. When a window was raised, put the
-- originally-frontmost window back on top to restore the prior order.
local function cancelPreview()
    if previewTimer then
        previewTimer:stop()
        previewTimer = nil
    end
    if previewActive and originalFront then
        originalFront:raise()
    end
    previewActive = false
end

-- Schedule a preview of the current selection after PREVIEW_DELAY. The raised
-- window sits behind the overlay, which stays above it at the overlay level.
local function schedulePreview()
    if previewTimer then previewTimer:stop() end
    previewTimer = hs.timer.doAfter(PREVIEW_DELAY, function()
        previewTimer = nil
        local win = windows[selectedIdx]
        if win and altIsDown and win ~= originalFront then
            win:raise()
            previewActive = true
        end
    end)
end

local function focusSelected()
    if previewTimer then
        previewTimer:stop()
        previewTimer = nil
    end
    previewActive = false
    local win = windows[selectedIdx]
    hideOverlay()
    if win then win:focus() end
end

local function redraw()
    local count  = math.min(#windows, MAX_ROWS)
    local height = count * ROW_H + PAD * 2
    local sf     = hs.screen.mainScreen():frame()
    local WIDTH  = math.floor(sf.w / 4)
    local ox     = math.floor((sf.w - WIDTH) / 2) + sf.x - MARGIN
    local oy     = math.floor((sf.h - height) / 2) + sf.y - MARGIN

    if overlay then overlay:delete() end
    overlay = hs.canvas.new({ x = ox, y = oy, w = WIDTH + MARGIN * 2, h = height + MARGIN * 2 })
    overlay:level(hs.canvas.windowLevels.overlay)
    overlay:alpha(1)

    local els = {}

    -- background
    els[#els + 1] = {
        type             = "rectangle",
        action           = "fill",
        fillColor        = { white = 0.97, alpha = 0.97 },
        roundedRectRadii = { xRadius = 12, yRadius = 12 },
        frame            = { x = MARGIN, y = MARGIN, w = WIDTH, h = height },
        withShadow       = true,
        shadow           = {
            blurRadius = 18,
            color      = { white = 0, alpha = 0.35 },
            offset     = { h = -6, w = 0 },
        },
    }

    for i, win in ipairs(windows) do
        if i > MAX_ROWS then break end
        local ry  = MARGIN + PAD + (i - 1) * ROW_H
        local sel = i == selectedIdx

        -- row highlight
        if sel then
            els[#els + 1] = {
                type             = "rectangle",
                action           = "fill",
                fillColor        = { red = 0.20, green = 0.50, blue = 0.95, alpha = 0.90 },
                roundedRectRadii = { xRadius = 7, yRadius = 7 },
                frame            = { x = MARGIN + PAD / 2, y = ry + 2, w = WIDTH - PAD, h = ROW_H - 4 },
            }
        end

        -- app icon
        local img = icon(win:application())
        if img then
            els[#els + 1] = {
                type  = "image",
                image = img,
                frame = {
                    x = MARGIN + PAD,
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
                color      = sel and { white = 1, alpha = 1.0 } or { white = 0.15, alpha = 0.85 },
                paragraphStyle = { lineBreak = "truncatingTail" },
            }),
            frame = {
                x = MARGIN + PAD + ICON_SZ + 10,
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
        windows       = visibleWindows()
        selectedIdx   = math.min(2, #windows)
        originalFront = windows[1]
        previewActive = false
        if #windows > 0 then
            redraw()
            schedulePreview()
        end
    else
        cancelPreview()
        selectedIdx = ((selectedIdx - 1 + dir) % #windows) + 1
        redraw()
        schedulePreview()
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
