Arc = require "xi.arc"
import compare from require "xi.utils"

--TODO: context stuff, arcEquals? is it right?

class Event
  new: (whole = nil, part = Arc(), value = nil, context = {}, stateful = false) =>
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

  withArc:(func) =>
    if @whole != nil
      Event func(@whole), func(@part), @value, @context, @stateful
    else
      Event nil, func(@part), @value, @context, @stateful

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

    string.format "[%s | %s]", partString, @value

  __eq:(other) =>
    (@part == other.part) and (@whole == other.whole) and (@value == other.value) and (compare @context, other.context) and (@stateful == other.stateful)

return Event
