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

for i, v in pairs pattern
  if i != "C" and i != "Pattern"
    xi[i] = v

for i, v in pairs pattern_factory
  xi[i] = v

for i, v in pairs C
  xi[i] = v

return xi
