require "xi.fraction"

export class Span
  new:(b = 1, e = 1) =>
    if type(b) == "number"
      b = Fraction(b)
    if type(e) == "number"
      e = Fraction(e)

    @_begin = b
    @_end = e

  type: -> 'span'

  spanCycles: =>
    spans = {}

    if @_begin == @_end
      { Span(@_begin, @_end) }

    while @_end > @_begin
      if @_begin\sam! == @_end\sam!
        table.insert(spans, Span(@_begin, @_end))
        break

      table.insert(spans, Span(@_begin, @_begin\nextSam!))
      @_begin = @_begin\nextSam!

    spans

  duration: => @_end - @_begin

  midpoint: => @_begin + (@duration! / 2)

  cycleSpan: => Span @cyclePos(@_begin), @cyclePos(@_begin) + @duration!

  __eq:(rhs) => @_begin == rhs._begin and @_end == rhs._end

  __tostring: => string.format "%s → %s", @_begin\show!, @_end\show!

  show: => @__tostring!

  withTime:(func) => Span func(@_begin), func(@_end)

  withEnd:(func) => Span @_begin, func(@_end)

  wholeCycle:(frac) => Span frac\sam!, frac\nextSam!

  cyclePos:(frac) => frac - frac\sam!

  sect:(other) =>
    maxOfStart = @_begin\max(other._begin)
    minOfEnd = @_end\min(other._end)

    if maxOfStart > minOfEnd
      return nil
    if maxOfStart == minOfEnd
      if maxOfStart == @_end and @_begin < @_end
        return nil
      if maxOfStart == other._end and other._begin < other._end
        return nil
    Span maxOfStart, minOfEnd

  sect_e:(other) =>
    result = @sect(other)
    if not result
      error("Span: Arcs do not intersect")
    result
