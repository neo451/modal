import slowcat, fastcat, pure, stack, timecat, randcat, fast, slow, degrade from require "xi.pattern"
import drawline from require "xi.drawline"
import hush, p, DefaultClock, d1, d2, d3, d4, d5, d6, d7, d8 from require "xi.pattern_factory"
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
xi.timecat = timecat
xi.randcat = randcat
xi.fast = fast
xi.slow = slow
xi.degrade = degrade
xi.drawline = drawline
xi.clock = DefaultClock
xi.d1 = d1
xi.d2 = d2
xi.d3 = d3
xi.d4 = d4
xi.d5 = d5
xi.d6 = d6
xi.d7 = d7
xi.d8 = d8

return xi
