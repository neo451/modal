import compare, dumpval, dump from require "xi.utils"
import Span from require "xi.span"

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
    (@part == other.part) and (@whole == other.whole) and (@value == other.value) and (compare @context, other.context) and (@stateful == other.stateful)

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

return { :Event }
