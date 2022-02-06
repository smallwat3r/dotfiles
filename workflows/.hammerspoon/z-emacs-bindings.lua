-- Trigger emacs client
hyper:bind({}, 'c', function()
  hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacsclient -a "" -c' }):start()
  hyper.triggered = true
end)

-- Trigger emacs-everywhere
hyper:bind({}, 'e', function()
  hs.task.new('/bin/bash', nil, { '-l', '-c', 'emacsclient -a "" --eval "(emacs-everywhere)"' }):start()
  hyper.triggered = true
end)
