socket = require "socket"
abletonlink = require "abletonlink"
import type from require "xi.utils"
sleep = (sec) -> socket.sleep sec

class Clock
  new:(bpm = 120, sampleRate = 1/20, beatsPerCycle = 4) =>
    @bpm, @sampleRate, @beatsPerCycle = bpm, sampleRate, beatsPerCycle
    -- . ?
    @link = abletonlink.create bpm
    @sessionState = abletonlink.create_session_state!
    @subscribers = {}
    @running = false
    @notifyCoroutine = nil

  type: -> "clock"

  start: =>
    if not @running
      @running = true
      @createNotifyCoroutine!

  stop: =>
    @running = false
    print "clock stopped"
    coroutine.close @notifyCoroutine

  subscribe:(subscriber) =>
    table.insert @subscribers, subscriber

  unsubscribe:(subscriber) =>
    position
    for i, sub in pairs @subscribers
      -- p type sub
      if sub == subscriber
        position = i
    if position != nil
      table.remove @subscribers, position

  createNotifyCoroutine: =>
    @notifyCoroutine = coroutine.create =>
      print "setup", @running
      @link\enable(true)
      @link\enable_start_stop_sync(true)

      start = @link\clock_micros!

      ticks = 0
      mill = 1000000
      frame = @sampleRate * mill

      print "OK..."

      while @running
        print "tick"
        ticks += 1

        logicalNow = math.floor start + (ticks * frame)
        logicalNext = math.floor start + ((ticks + 1) * frame)

        wait = (logicalNow - now) / mill

        now = @link\clock_micros!

        if wait > 0
          sleep wait

        print "running is set to ", @running

        if not @running
          break

        @link\capture_audio_seesionState @sessionState
        secondsPerMinute = 60
        cps = (@seesionState\tempo! / @beatsPerCycle) / secondsPerMinute
        cycleFrom = @sessionState\beat_at_time(logicalNow, 0) / @beatsPerCycle
        cycleTo = @sessionState\beat_at_time(logicalNext, 0) / @beatsPerCycle

        for sub in *@subscribers
          sub\notifyTick cycleFrom, cycleTo, @sessionState, cps, @beatsPerCycle, mill, now
        print("the coroutine is running? ... ", coroutine.running!)
        coroutine.yield!
      @linkEnabled = false --?

return { Clock: Clock }
