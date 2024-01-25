socket = require "socket"
link = require "abletonlink"
Timetag = require "losc.timetag"
import type from require "xi.utils"
sleep = (sec) -> socket.sleep sec

class Clock
  new:(bpm = 120, sampleRate = 1/20, beatsPerCycle = 4) =>
    @bpm, @sampleRate, @beatsPerCycle = bpm, sampleRate, beatsPerCycle
    @link = link.create bpm
    @sessionState = link.create_session_state!
    @subscribers = {}
    @running = false
    @notifyCoroutine = nil
    @latency = 0.2

  type: -> "clock"

  start: =>
    if not @running
      @running = true
      @createNotifyCoroutine!

  stop: =>
    @running = false
    print "clock stopped"
    -- coroutine.close @notifyCoroutine

  subscribe:(subscriber) =>
    table.insert @subscribers, subscriber

  unsubscribe:(subscriber) =>
    position = nil
    for i, sub in ipairs @subscribers
      if sub == subscriber
        position = i
    if position != nil
      table.remove @subscribers, position

  createNotifyCoroutine: =>
    @notifyCoroutine = coroutine.create ->
      @link\enable(true)
      @link\enable_start_stop_sync(true)

      start = @link\clock_micros!

      ticks = 0
      mill = 1000000
      frame = @sampleRate * mill

      while @running
        ticks += 1

        logicalNow = math.floor start + (ticks * frame)

        logicalNext = math.floor start + ((ticks + 1) * frame)

        now = @link\clock_micros!

        wait = (logicalNow - now) / mill

        if wait > 0 then sleep wait

        if not @running then break

        @link\capture_audio_session_state @sessionState


        cps = (@sessionState\tempo! / @beatsPerCycle) / 60

        cycleFrom = @sessionState\beat_at_time(logicalNow, 0) / @beatsPerCycle

        cycleTo = @sessionState\beat_at_time(logicalNext, 0) / @beatsPerCycle

        for sub in *@subscribers
          sub\notifyTick cycleFrom, cycleTo, @sessionState, cps, @beatsPerCycle, mill, now
        coroutine.yield!

      @linkEnabled = false

return { :Clock }
