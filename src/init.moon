pattern = require "modal.pattern"
pattern_factory = require "modal.pattern_factory"
C = require "modal.control"
import drawline from require "modal.drawline"

modal = {
  _VERSION: "modal dev-1"
	_URL: "https://github.com/noearc/modal"
	_DESCRIPTION: "A language for algorithmic pattern. Tidalcycles for moonscript"
}

modal.drawline = drawline

for name, func in pairs pattern
  if name != "Pattern"
    modal[name] = func

for name, func in pairs pattern_factory
  modal[name] = func

for name, func in pairs C
  modal[name] = func

setmetatable(modal, {
  __call: (t, override) ->
    for k, v in pairs(t)
      if _G[k] ~= nil
        msg = 'function ' .. k .. ' already exists in global scope.'
        if override
          _G[k] = v
          print('WARNING: ' .. msg .. ' Overwritten.')
        -- else
        --   print('NOTICE: ' .. msg .. ' Skipped.')
      else
        _G[k] = v
})
return modal
