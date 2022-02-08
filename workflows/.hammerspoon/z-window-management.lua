hs.window.animationDuration=0

local hotkey = require "hs.hotkey"
local grid = require "hs.grid"

grid.MARGINX = 5
grid.MARGINY = 5
grid.GRIDHEIGHT = 3
grid.GRIDWIDTH = 3

-- Center
hotkey.bind(
  mod_cmd,
  '[',
  function()
    hs.window.focusedWindow():centerOnScreen()
  end
)

-- Fullscreen
hotkey.bind(
  mod_cmd,
  ']',
  function()
    hs.window.focusedWindow():maximize(0)
  end
)

--Move
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
hotkey.bind(
  mod_ctrl_cmd,
  'u',
  function()
    hs.hints.windowHints()
  end
)
