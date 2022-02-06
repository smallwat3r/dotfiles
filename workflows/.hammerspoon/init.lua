-- smallwat3r's hammerspoon config

-- Karabiner is mapping a virtual F18 key to caps-lock, so we can use
-- it from Hammerspoon to enter hyper mode

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

f18 = hs.hotkey.bind({}, 'F18', enterHyperMode, exitHyperMode)

require('z-emacs-bindings')
require('z-ctrl-hjkl-to-arrows')
