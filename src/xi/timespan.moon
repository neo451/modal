Fraction = require "xi.fraction"

class Timespan
  new:(b, e) =>
    if type(b) == "number"
      b = Fraction(b)
    if type(e) == "number"
      e = Fraction(e)

    @_begin = b
    @_end = e

  type: => 'timespan'

-- HACK:
  spanCycles: =>
    spans = {}

    if @_begin == @_end
      { Timespan(@_begin, @_end) }

    while @_end > @_begin
      if @_begin\sam! == @_end\sam!
        spans:insert(Timsspan(@_begin, @_end))
        break

      begin = @_begin
      next_begin = @_begin\nextSam!

      spans:insert(Timespan(begin, next_begin))

      begin = next_begin

    spans

  duration: => @_end - @_begin

  midpoint: => @_begin + (@duration! / Fraction(2))

  cycleArc: => Timespan cyclePos(@_begin), cyclePos(@_begin) + @duration

  __eq:(rhs) => @_begin == rhs.begin and @_end == rhs.end

  __tostring: => string.format "%s → %s", @_begin\show!, @_end\show!

  show: => @__tostring!

  withTime:(func) => Timespan func(@_begin), func(@_end)

  withEnd:(func) => Timespan @_begin, func(@_end)

  wholeCycle:(frac) => Timespan frac\sam!, frac\nextSam!

  cyclePos:(frac) => frac - frac\sam!

return Timespan
