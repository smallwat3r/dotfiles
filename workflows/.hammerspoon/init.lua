-- smallwat3r's hammerspoon config

mod_alt = {'alt'}
mod_cmd = {'cmd'}
mod_ctrl = {'ctrl'}
mod_ctrl_cmd = {'ctrl', 'cmd'}

local hotkey = require "hs.hotkey"
local grid = require "hs.grid"

-- ***
-- Emacs stuff
-- ***

-- Spawn a new emacs client
local function newEmacsClient()
  hs.alert.show("New Emacs client!")
  hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacsclient -a "" -c' }):start()
end

hotkey.bind(mod_cmd, 'e', function() newEmacsClient() end)

-- Spawn an instance of emacs-everywhere
local function emacsEverywhere()
  hs.alert.show("Emacs everywhere!")
  hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacsclient -a "" --eval "(emacs-everywhere)"' }):start()
end

hotkey.bind(mod_cmd, '.', function() emacsEverywhere() end)

-- Kill the running emacs daemon with confirmation
local function confirmationDialog(actionFunc)
  test = hs.chooser.new(actionFunc)
  test:rows(2)
  test:choices({
    {["text"] = "Yes", ["id"] = "yes", ["subText"] = "Kill the running Emacs daemon"},
    {["text"] = "No", ["id"] = "no", ["subText"] = "Leave the Emacs daemon running"}
  })
  test:show()
end

local function stopEmacsDaemon(input)
  if input and input.id == "yes" then
    hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacsclient -e "(kill-emacs)"' }):start()
    hs.alert.show("Stopped Emacs daemon!")
  end
end

hotkey.bind(mod_cmd, '§', function() confirmationDialog(stopEmacsDaemon) end)

-- ***
-- Make the combination of Ctrl + hjkl to emulate the arrow keys behaviour.
-- It emulates the vim bindings to go left, up, down or right. This is very
-- useful as it fits my workflow in Emacs when browsing up or down up menus.
-- ***

local function pressFn(mods, key)
  if key == nil then
    key = mods
    mods = {}
  end

  return function()
    hs.eventtap.keyStroke(mods, key, 1000)
  end
end

local function remap(mods, key, pressFn)
  hs.hotkey.bind(mods, key, pressFn, nil, pressFn)
end

remap(mod_alt, 'h', pressFn('left'))
remap(mod_alt, 'j', pressFn('down'))
remap(mod_alt, 'k', pressFn('up'))
remap(mod_alt, 'l', pressFn('right'))

-- ***
-- Window management
-- ***

hs.window.animationDuration = 0

grid.MARGINX = 2
grid.MARGINY = 2
grid.GRIDHEIGHT = 4
grid.GRIDWIDTH = 4

-- Center window
hotkey.bind(mod_cmd, '[', function() hs.window.focusedWindow():centerOnScreen() end)

-- Fullscreen window
hotkey.bind(mod_cmd, ']', function() hs.window.focusedWindow():maximize(0) end)

-- Move window
hotkey.bind(mod_ctrl_cmd, 'j', grid.pushWindowDown)
hotkey.bind(mod_ctrl_cmd, 'k', grid.pushWindowUp)
hotkey.bind(mod_ctrl_cmd, 'h', grid.pushWindowLeft)
hotkey.bind(mod_ctrl_cmd, 'l', grid.pushWindowRight)

-- Resize window
hotkey.bind(mod_cmd, 'k', grid.resizeWindowShorter)
hotkey.bind(mod_cmd, 'j', grid.resizeWindowTaller)
hotkey.bind(mod_cmd, 'l', grid.resizeWindowWider)
hotkey.bind(mod_cmd, 'h', grid.resizeWindowThinner)

-- Show window hints
hotkey.bind(mod_alt, 'Tab', function() hs.hints.windowHints() end)
