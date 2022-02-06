-- Trigger new emacs client
hyper:bind({}, 'n', function()
  hs.task.new('/usr/bin/osascript', nil, { '-e', 'display notification "New Emacs client!"' }):start()
  hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacsclient -a "" -c' }):start()
  hyper.triggered = true
end)

-- Trigger emacs-everywhere
hyper:bind({}, 'e', function()
  hs.task.new('/usr/bin/osascript', nil, { '-e', 'display notification "Emacs everywhere!"' }):start()
  hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacsclient -a "" --eval "(emacs-everywhere)"' }):start()
  hyper.triggered = true
end)

-- Start emacs daemon
hyper:bind({}, 's', function()
  hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacs --daemon' }):start()
  hs.task.new('/usr/bin/osascript', nil, { '-e', 'display notification "Started Emacs daemon!"' }):start()
  hyper.triggered = true
end)

-- Kill emacs daemon
hyper:bind({}, 'k', function()
  hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacsclient -e "(kill-emacs)"' }):start()
  hs.task.new('/usr/bin/osascript', nil, { '-e', 'display notification "Stopped Emacs daemon!"' }):start()
  hyper.triggered = true
end)
