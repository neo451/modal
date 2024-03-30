import Clock from require "modal.clock"
require"moon.all"

class Stream
  new:(sendf) =>
    @isPlaying = false
    @latency = 0.3
    @pattern = nil
    @sendf = sendf

  type: -> "stream"

  notifyTick: (cycleFrom, cycleTo, s, cps, bpc, mill, now) =>
    if not @pattern
      return

    events = @pattern\onsetsOnly!\querySpan cycleFrom, cycleTo

    -- print "cycle from: ", cycleFrom, " ", "cycle to: ", cycleTo

    for event in *events
      cycleOn = event.whole._begin
      cycleOff = event.whole._end
      linkOn = s\time_at_beat cycleOn\asFloat! * bpc, 0
      linkOff = s\time_at_beat cycleOff\asFloat! * bpc, 0
      deltaSeconds = (linkOff - linkOn) / mill
      linkSecs = now / mill
      -- libloDiff = losc\now! + (-linkSecs)
      -- ts = libloDiff + @latency + (linkOn / mill)

      value = event.value
      value.cps = event.value.cps or cps --???
      value.cycle = cycleOn\asFloat!
      value.delta = deltaSeconds

      @sendf value

return { :Stream }
