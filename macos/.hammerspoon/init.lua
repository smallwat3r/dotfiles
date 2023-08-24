-- smallwat3r"s hammerspoon config

local hotkey = require "hs.hotkey"
local grid = require "hs.grid"
local hints = require "hs.hints"
local window = require "hs.window"
local alert = require "hs.alert"
local task = require "hs.task"
local eventtap = require "hs.eventtap"
local chooser = require "hs.chooser"

mod_alt = {"alt"}
mod_cmd = {"cmd"}
mod_ctrl = {"ctrl"}
mod_ctrl_cmd = {"ctrl", "cmd"}
mod_ctrl_alt = {"ctrl", "alt"}

alert.defaultStyle = {
    strokeWidth  = 5,
    strokeColor = { white = 1, alpha = 1 },
    fillColor = { white = 0, alpha = 1 },
    textColor = { white = 1, alpha = 1 },
    textFont = "Triplicate B",
    textSize = 16,
    radius = 0,
    atScreenEdge = 0,
    fadeInDuration = 0,
    fadeOutDuration = 0,
    padding = nil,
}

-- Emacs stuff
-- ***************************************************************************

-- Spawn a new emacs client
local function newEmacsClient()
  alert.show("New Emacs client!")
  task.new("/bin/zsh", nil,
           { "-l", "-c", "emacsclient -a '' -c" }):start()
end

hotkey.bind(mod_cmd, "e", function() newEmacsClient() end)

-- Spawn an instance of emacs-everywhere
local function emacsEverywhere()
  alert.show("Emacs everywhere!")
  task.new("/bin/zsh", nil,
           { "-l", "-c", 'emacsclient -a "" --eval "(emacs-everywhere)"' }):start()
end

-- TODO: remap this, as I keep hitting it by mistake, also should I keep using this?
-- hotkey.bind(mod_cmd, ".", function() emacsEverywhere() end)

-- Kill the running emacs daemon with confirmation
local function confirmationDialog(actionFunc)
  test = chooser.new(actionFunc)
  test:rows(2)
  test:choices({
    {["text"] = "Yes", ["id"] = "yes", ["subText"] = "Kill the running Emacs daemon"},
    {["text"] = "No", ["id"] = "no", ["subText"] = "Leave the Emacs daemon running"}
  })
  test:show()
end

local function stopEmacsDaemon(input)
  if input and input.id == "yes" then
    task.new("/bin/zsh", nil,
             { "-l", "-c", 'emacsclient -e "(kill-emacs)"' }):start()
    alert.show("Stopped Emacs daemon!")
  end
end

hotkey.bind(mod_cmd, "`", function() confirmationDialog(stopEmacsDaemon) end)

-- Window management
-- ***************************************************************************

window.animationDuration = 0

grid.MARGINX = 2
grid.MARGINY = 2
grid.GRIDHEIGHT = 4
grid.GRIDWIDTH = 4

-- Center window
hotkey.bind(mod_cmd, "[", function() window.focusedWindow():centerOnScreen() end)

-- Fullscreen window
hotkey.bind(mod_cmd, "]", function() window.focusedWindow():maximize(0) end)

-- Move window
hotkey.bind(mod_ctrl_alt, "j", grid.pushWindowDown)
hotkey.bind(mod_ctrl_alt, "k", grid.pushWindowUp)
hotkey.bind(mod_ctrl_alt, "h", grid.pushWindowLeft)
hotkey.bind(mod_ctrl_alt, "l", grid.pushWindowRight)

-- Resize window
hotkey.bind(mod_alt, "k", grid.resizeWindowShorter)
hotkey.bind(mod_alt, "j", grid.resizeWindowTaller)
hotkey.bind(mod_alt, "l", grid.resizeWindowWider)
hotkey.bind(mod_alt, "h", grid.resizeWindowThinner)

-- Show window hints
hints.style = "vimperator"
hotkey.bind(mod_alt, "Tab", function() hints.windowHints() end)

-- Make the combination of Cmd + hjkl to emulate the arrow keys behaviour.
-- ***************************************************************************

local function pressFn(mods, key)
  if key == nil then
    key = mods
    mods = {}
  end

  return function()
    eventtap.keyStroke(mods, key, 1000)
  end
end

local function remap(mods, key, pressFn)
  hotkey.bind(mods, key, pressFn, nil, pressFn)
end

remap(mod_cmd, "h", pressFn("left"))
remap(mod_cmd, "j", pressFn("down"))
remap(mod_cmd, "k", pressFn("up"))
remap(mod_cmd, "l", pressFn("right"))

-- Launcher
-- ***************************************************************************

-- app launcher
local function chooseApp()
  task.new("/bin/zsh", nil, { "-l", "-c", "choose-app" }):start()
end

hotkey.bind(mod_cmd, ".", function() chooseApp() end)

-- bin launcher
local function chooseBin()
  task.new("/bin/zsh", nil, { "-l", "-c", "choose-bin" }):start()
end

hotkey.bind(mod_cmd, ",", function() chooseBin() end)

-- google chrome history launcher
local function chooseChromeHistory()
  task.new("/bin/zsh", nil, { "-l", "-c", "choose-chrome-history" }):start()
end

hotkey.bind(mod_cmd, "/", function() chooseChromeHistory() end)
