--TODO: intersection, intersection_e
Fraction = require "xi.fraction"

class Timespan
  new:(b = 1, e = 1) =>
    if type(b) == "number"
      b = Fraction(b)
    if type(e) == "number"
      e = Fraction(e)

    @_begin = b
    @_end = e

  type: => 'timespan'

  spanCycles: =>
    spans = {}

    if @_begin == @_end
      { Timespan(@_begin, @_end) }

    while @_end > @_begin
      if @_begin\sam! == @_end\sam!
        table.insert(spans, Timespan(@_begin, @_end))
        break

      table.insert(spans, Timespan(@_begin, @_begin\nextSam!))
      @_begin = @_begin\nextSam!

    spans

  duration: => @_end - @_begin

  midpoint: => @_begin + (@duration! / Fraction(2))

  cycleArc: => Timespan @cyclePos(@_begin), @cyclePos(@_begin) + @duration!

  __eq:(rhs) => @_begin == rhs.begin and @_end == rhs.end

  __tostring: => string.format "%s → %s", @_begin\show!, @_end\show!

  show: => @__tostring!

  withTime:(func) => Timespan func(@_begin), func(@_end)

  withEnd:(func) => Timespan @_begin, func(@_end)

  wholeCycle:(frac) => Timespan frac\sam!, frac\nextSam!

  cyclePos:(frac) => frac - frac\sam!

return Timespan
