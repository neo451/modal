require "xi.pattern"

xi = {
  _VERSION: "xi dev-1",
	-- _URL = "https://github.com/xinniw/tranquility",
	_DESCRIPTION: "A language for algorithmic pattern. Tidalcycles for moonscript",
}

xi.cat = slowcat
xi.seq = fastcat
xi.pure = pure
xi.stack = stack

return xi
