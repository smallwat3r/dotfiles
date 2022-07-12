local hotkey = require "hs.hotkey"
local grid = require "hs.grid"

hs.window.animationDuration = 0

grid.MARGINX = 2
grid.MARGINY = 2
grid.GRIDHEIGHT = 4
grid.GRIDWIDTH = 4

-- Center
hotkey.bind(mod_cmd, '[', function() hs.window.focusedWindow():centerOnScreen() end)

-- Fullscreen
hotkey.bind(mod_cmd, ']', function() hs.window.focusedWindow():maximize(0) end)

-- Move
hotkey.bind(mod_ctrl_cmd, 'j', grid.pushWindowDown)
hotkey.bind(mod_ctrl_cmd, 'k', grid.pushWindowUp)
hotkey.bind(mod_ctrl_cmd, 'h', grid.pushWindowLeft)
hotkey.bind(mod_ctrl_cmd, 'l', grid.pushWindowRight)

-- Resize
hotkey.bind(mod_cmd, 'k', grid.resizeWindowShorter)
hotkey.bind(mod_cmd, 'j', grid.resizeWindowTaller)
hotkey.bind(mod_cmd, 'l', grid.resizeWindowWider)
hotkey.bind(mod_cmd, 'h', grid.resizeWindowThinner)

-- Hints
hotkey.bind(mod_alt, 'Tab', function() hs.hints.windowHints() end)
