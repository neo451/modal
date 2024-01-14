import slowcat, fastcat, pure, stack from require "xi.pattern"
import hush, p, DefaultClock from require "xi.pattern_factory"
import sound from require "xi.control"
import Clock from require "xi.clock"

xi = {
  _VERSION: "xi dev-1",
	-- _URL = "https://github.com/xinniw/tranquility",
	_DESCRIPTION: "A language for algorithmic pattern. Tidalcycles for moonscript",
}

-- xi._index = xi
xi.p = p
xi.s = sound
xi.hush = hush
xi.cat = slowcat
xi.seq = fastcat
xi.pure = pure
xi.stack = stack
xi.clock = DefaultClock

return xi
