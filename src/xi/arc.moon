Fraction = require "xi.fraction"

class Arc
  new:(b = 1, e = 1) =>
    if type(b) == "number"
      b = Fraction(b)
    if type(e) == "number"
      e = Fraction(e)

    @_begin = b
    @_end = e

  type: -> 'arc'

  spanCycles: =>
    spans = {}

    if @_begin == @_end
      { Arc(@_begin, @_end) }

    while @_end > @_begin
      if @_begin\sam! == @_end\sam!
        table.insert(spans, Arc(@_begin, @_end))
        break

      table.insert(spans, Arc(@_begin, @_begin\nextSam!))
      @_begin = @_begin\nextSam!

    spans

  duration: => @_end - @_begin

  midpoint: => @_begin + (@duration! / Fraction(2))

  cycleArc: => Arc @cyclePos(@_begin), @cyclePos(@_begin) + @duration!

  __eq:(rhs) => @_begin == rhs._begin and @_end == rhs._end

  __tostring: => string.format "%s → %s", @_begin\show!, @_end\show!

  show: => @__tostring!

  withTime:(func) => Arc func(@_begin), func(@_end)

  withEnd:(func) => Arc @_begin, func(@_end)

  wholeCycle:(frac) => Arc frac\sam!, frac\nextSam!

  cyclePos:(frac) => frac - frac\sam!

  intersection:(other) =>
    startOfIntersection = @_begin\max(other._begin)
    endOfIntersection = @_end\min(other._end)

    if startOfIntersection > endOfIntersection
      return nil
    if startOfIntersection == endOfIntersection
      if startOfIntersection == @_end and @_begin < @_end
        return nil
      if startOfIntersection == other._end and other._begin < other._end
        return nil
    Arc startOfIntersection, endOfIntersection

  intersection_e:(other) =>
    result = @intersection(other)
    print result
    if not result
      error("Arc: Arcs do not intersect")
    result

return Arc
