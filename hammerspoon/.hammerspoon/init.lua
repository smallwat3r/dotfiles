hyper = hs.hotkey.modal.new({}, 'F17')

function enterHyperMode()
  hyper.triggered = false
  hyper:enter()
end

function exitHyperMode()
  hyper:exit()
  if not hyper.triggered then
    hs.eventtap.keyStroke({}, 'ESCAPE')
  end
end

-- Karabiner is mapping F18 to caps-lock, this works as our hyper key
f18 = hs.hotkey.bind({}, 'F18', enterHyperMode, exitHyperMode)

-- Trigger emacs-everywhere
hyper:bind({}, "e", function()
  hs.task.new("/bin/bash", nil, { "-l", "-c", "emacsclient --eval '(emacs-everywhere)'" }):start()
  hyper.triggered = true
end)
