-- Make the combination of Ctrl + hjkl to emulate the arrow keys behaviour.
-- It emulates the vim bindings to go left, up, down or right. This is very
-- useful as it fits my workflow in Emacs when browsing up or down up or down
-- menus.

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

remap(mod_ctrl, 'h', pressFn('left'))
remap(mod_ctrl, 'j', pressFn('down'))
remap(mod_ctrl, 'k', pressFn('up'))
remap(mod_ctrl, 'l', pressFn('right'))
