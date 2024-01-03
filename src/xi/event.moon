Arc = require "xi.arc"

--TODO: context stuff, spanEquals? is it right?

class Event
  new: (whole = nil, part = Arc(), value = nil, context = {}, stateful = false) =>
    if stateful and type(value) ~= "function"
      error("Event: stateful event values must be of type function")

    @whole, @part, @value, @context, @stateful = whole, part, value, context, stateful

  type: -> "event"

  duration: => @whole._end - @whole._begin

  wholeOrPart: =>
    if @whole != nil
      @whole
    @part

  hasWhole: => @whole != nil

  -- if EVENT is NIL?
  withSpan:(func) =>
    Event func(@whole), func(@part), @value, @context, @stateful

  withValue:(func) =>
    Event @whole, @part, func(@value), @context, @stateful

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

    --TODO: CompareTables
  __eq:(other) =>
    @part == other.part and @whole == other.whole and @value == other.value and @context == other.context and @stateful == other.stateful

return Event
