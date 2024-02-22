import Fraction, tofrac from require "xi.fraction"
import compare, dump from require "xi.utils"

-- types = require "types"

class Span
  new:(b = 1, e = 1) =>
    @_begin, @_end = tofrac(b), tofrac(e)

  type: -> "span"

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

  withCycle:(func) =>
    sam = @_begin\sam!
    b = sam + func(@_begin - sam)
    e = sam + func(@_end - sam)
    return Span b, e

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

class Event
  new: (whole = nil, part = Span!, value = nil, context = {}, stateful = false) =>
    if stateful and type(value) ~= "function"
      error("Event: stateful event values must be of type function")

    @whole, @part, @value, @context, @stateful = whole, part, value, context, stateful

  type: -> "event"

  duration: => @whole._end - @whole._begin

  wholeOrPart: =>
    if @whole != nil
      return @whole
    return @part

  hasWhole: => @whole != nil

  hasOnset: => @whole != nil and @whole._begin == @part._begin

  withSpan:(func) =>
    whole = @whole
    if whole != nil
      whole = func whole
    Event whole, func(@part), @value, @context, @stateful

  withValue:(func) => Event @whole, @part, func(@value), @context, @stateful

  show: => @__tostring!

  __tostring: =>
    partStartsWithWhole = @hasWhole! and @whole._begin == @part._begin
    partEndsWithWhole = @hasWhole! and @whole._end == @part._end
    partFormat = "(%s)"

    if partStartsWithWhole and partEndsWithWhole
      partFormat = "%s"
    elseif partStartsWithWhole
      partFormat = "(%s) ⇝"
    else
      partFormat = "(%s) ⇜"

    partString = string.format partFormat, @part\show!

    string.format "[%s | %s]", partString, dump @value

  spanEquals: (other) =>
    ((other.whole == nil) and (@whole == nil)) or (other.whole == @whole)

  __eq:(other) =>
    (@part == other.part) and (@whole == other.whole) and (compare @value, other.value) and (compare @context, other.context) and (@stateful == other.stateful)

  setContext:(newContext) =>
    Event @whole, @part, @value, newContext, @stateful

  combineContext:(other) =>
    newContext = {}
    for key, value in pairs @context
      newContext[key] = value
    for key, value in pairs other.context
      newContext[key] = value
    loc1 = @context.locations or {}
    loc2 = other.context.locations or {}
    newloc = {}
    for _, value in ipairs loc1
      table.insert newloc, value
    for _, value in ipairs loc2
      table.insert newloc, value
    newContext.locations = newloc
    newContext

class State
  new:(span = Span!, controls = {}) =>
    @span = span
    @controls = controls

  type: -> "state"

  setSpan:(span) => State span, @controls

  withSpan:(func) => @setSpan(func @span)

  setControls:(controls) => State @span, controls

  __tostring: => "span: ".. @span\show! .. "  controls: " .. dump @controls

  __eq:(other) => @span == other.span and compare @controls, other.controls

return {
  :Span
  :Event
  :State
}
