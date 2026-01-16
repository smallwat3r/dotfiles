-- smallwat3r's hammerspoon config

local hotkey = require 'hs.hotkey'
local grid = require 'hs.grid'
local window = require 'hs.window'
local alert = require 'hs.alert'
local task = require 'hs.task'
local eventtap = require 'hs.eventtap'
local chooser = require 'hs.chooser'

local mod_shift_alt = {'shift', 'alt'}
local mod_alt = {'alt'}
local mod_cmd = {'cmd'}
local mod_ctrl = {'ctrl'}
local mod_ctrl_alt = {'ctrl', 'alt'}

alert.defaultStyle = {
  strokeWidth = 1,
  strokeColor = { white = 1, alpha = 1 },
  fillColor = { white = 0, alpha = 1 },
  textColor = { white = 1, alpha = 1 },
  textFont = 'Triplicate B Code',
  textSize = 16,
  radius = 0,
  atScreenEdge = 0,
  fadeInDuration = 0,
  fadeOutDuration = 0,
  padding = nil,
}

-- Emacs

local function newEmacsClient()
  alert.show('New Emacs client!')
  task.new('/bin/zsh', nil, { '-l', '-c', 'emacsclient -a "" -c' }):start()
end

local function stopEmacsDaemon(input)
  if input and input.id == 'yes' then
    task.new('/bin/zsh', nil, { '-l', '-c', 'emacsclient -e "(kill-emacs)"' }):start()
    alert.show('Stopped Emacs daemon!')
  end
end

local function confirmKillEmacs()
  local dialog = chooser.new(stopEmacsDaemon)
  dialog:rows(2)
  dialog:choices({
    { text = 'Yes', id = 'yes', subText = 'Kill the running Emacs daemon' },
    { text = 'No', id = 'no', subText = 'Leave the Emacs daemon running' }
  })
  dialog:show()
end

hotkey.bind(mod_cmd, 'e', newEmacsClient)
hotkey.bind(mod_cmd, '`', confirmKillEmacs)

-- Window management

window.animationDuration = 0

grid.MARGINX = 2
grid.MARGINY = 2
grid.GRIDHEIGHT = 4
grid.GRIDWIDTH = 4

hotkey.bind(mod_ctrl_alt, ']', function() window.focusedWindow():centerOnScreen() end)
hotkey.bind(mod_alt, ']', function() window.focusedWindow():maximize(0) end)

-- Move window (hjkl + custom layout)
hotkey.bind(mod_ctrl_alt, 'j', grid.pushWindowDown)
hotkey.bind(mod_ctrl_alt, 'k', grid.pushWindowUp)
hotkey.bind(mod_ctrl_alt, 'h', grid.pushWindowLeft)
hotkey.bind(mod_ctrl_alt, 'l', grid.pushWindowRight)
hotkey.bind(mod_ctrl_alt, 'n', grid.pushWindowDown)
hotkey.bind(mod_ctrl_alt, 'a', grid.pushWindowUp)
hotkey.bind(mod_ctrl_alt, 'y', grid.pushWindowLeft)
hotkey.bind(mod_ctrl_alt, 'e', grid.pushWindowRight)

-- Resize window (hjkl + custom layout)
hotkey.bind(mod_alt, 'k', grid.resizeWindowShorter)
hotkey.bind(mod_alt, 'j', grid.resizeWindowTaller)
hotkey.bind(mod_alt, 'l', grid.resizeWindowWider)
hotkey.bind(mod_alt, 'h', grid.resizeWindowThinner)
hotkey.bind(mod_alt, 'a', grid.resizeWindowShorter)
hotkey.bind(mod_alt, 'n', grid.resizeWindowTaller)
hotkey.bind(mod_alt, 'e', grid.resizeWindowWider)
hotkey.bind(mod_alt, 'y', grid.resizeWindowThinner)

-- Key remapping (Cmd + hjkl -> arrow keys)

local function pressFn(mods, key)
  if key == nil then
    key = mods
    mods = {}
  end
  return function() eventtap.keyStroke(mods, key, 1000) end
end

local function remap(mods, key, fn)
  hotkey.bind(mods, key, fn, nil, fn)
end

remap(mod_cmd, 'h', pressFn('left'))
remap(mod_cmd, 'j', pressFn('down'))
remap(mod_cmd, 'k', pressFn('up'))
remap(mod_cmd, 'l', pressFn('right'))
remap(mod_cmd, 'delete', pressFn('forwarddelete'))

-- Custom launchers

local function launcherRunner(windowName, scriptLauncher, alacrittyOptions)
  local win = window.get(windowName)
  if win then
    win:focus()
    return
  end
  local command
  if alacrittyOptions then
    command = string.format(
      'INSIDE_HS=1 alacritty -T "%s" --config-file $HOME/.config/launcher.toml -o %s -e %s',
      windowName, alacrittyOptions, scriptLauncher)
  else
    command = string.format(
      'INSIDE_HS=1 alacritty -T "%s" --config-file $HOME/.config/launcher.toml -e %s',
      windowName, scriptLauncher)
  end
  task.new('/bin/zsh', nil, { '-l', '-c', command }):start()
end

hotkey.bind(mod_cmd, 'm', function() launcherRunner('App launcher', 'launcher-app') end)
hotkey.bind(mod_cmd, ',', function() launcherRunner('Bin launcher', 'launcher-bin') end)
hotkey.bind(mod_cmd, 'h', function() launcherRunner('History launcher', 'launcher-browser-history') end)
hotkey.bind(mod_cmd, 'space', function() launcherRunner('App switcher', 'launcher-running-app') end)
hotkey.bind(mod_cmd, ';', function() launcherRunner('File launcher', 'launcher-file-search') end)
hotkey.bind(mod_cmd, 'g', function() launcherRunner('Google search', 'chrome-search', 'window.dimensions.lines=3') end)
