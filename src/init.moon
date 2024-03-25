pattern = require "xi.pattern"
pattern_factory = require "xi.pattern_factory"
import C from require "xi.pattern"
import drawline from require "xi.drawline"

xi = {
  _VERSION: "xi dev-1"
	_URL: "https://github.com/noearc/xi"
	_DESCRIPTION: "A language for algorithmic pattern. Tidalcycles for moonscript"
}

xi.drawline = drawline

for name, func in pairs pattern
  if name != "C" and name != "Pattern"
    xi[name] = func

for name, func in pairs pattern_factory
  xi[name] = func

for name, func in pairs C
  xi[name] = func

setmetatable(xi, {
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
return xi
