local hotkey = require "hs.hotkey"

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
