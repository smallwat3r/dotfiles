local hotkey = require "hs.hotkey"

-- New emacs client
hotkey.bind(
  mod_cmd,
  'e',
  function()
    hs.task.new('/usr/bin/osascript', nil, { '-e', 'display notification "New Emacs client!"' }):start()
    hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacsclient -a "" -c' }):start()
  end
)

-- Trigger emacs-everywhere
hotkey.bind(
  mod_cmd,
  '.',
  function()
    hs.task.new('/usr/bin/osascript', nil, { '-e', 'display notification "Emacs everywhere!"' }):start()
    hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacsclient -a "" --eval "(emacs-everywhere)"' }):start()
  end
)

-- Kill emacs daemon
hotkey.bind(
  mod_cmd,
  'x',
  function()
    hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacsclient -e "(kill-emacs)"' }):start()
    hs.task.new('/usr/bin/osascript', nil, { '-e', 'display notification "Stopped Emacs daemon!"' }):start()
  end
)
