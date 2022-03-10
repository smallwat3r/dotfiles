local hotkey = require "hs.hotkey"

-- New emacs client
hotkey.bind(
  mod_cmd,
  'e',
  function()
    hs.alert.show("New Emacs client!")
    hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacsclient -a "" -c' }):start()
  end
)

-- Trigger emacs-everywhere
hotkey.bind(
  mod_cmd,
  '.',
  function()
    hs.alert.show("Emacs everywhere!")
    hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacsclient -a "" --eval "(emacs-everywhere)"' }):start()
  end
)

-- Kill emacs daemon
hotkey.bind(
  mod_cmd,
  '§',
  function()
    hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacsclient -e "(kill-emacs)"' }):start()
    hs.alert.show("Stopped Emacs daemon!")
  end
)
