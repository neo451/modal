socket = require "socket"
link = require "abletonlink"
losc = require "losc"
plugin = require "losc.plugins.udp-socket"
bundle = require "losc.bundle"

sleep = (sec) -> socket.sleep sec

target = { name: "SuperDirt", address: "127.0.0.1", port: 57120, latency: 0.2, handshake: true }

typeMap = {
  table: "b",
  number: "f",
  string: "s",
}

typesString = (msg) ->
  types = ""
  for x in *msg do
    if typeMap[type(x)] then
      types = types .. typeMap[type(x)]
    else
      types = types .. "b"
  return types

osc = losc.new(
  { plugin: plugin.new({ sendPort: target.port, sendAddr: target.address })}
)

sendOSC = (_, value, ts) ->
  msg = {}
  for key, val in pairs value do
    table.insert msg, key
    table.insert msg, val

  msg.types = typesString msg
  msg.address = "/dirt/play"

  b = osc.new_message msg

  -- bundle = bundle.new(ts, b)

  -- @osc\send bundle
  osc\send b

-- sendOSC { type: "clock", action: "start" }

class Clock
  @sendf = sendOSC
  new:(bpm = 120, sampleRate = 1/20, beatsPerCycle = 4) =>
    @bpm, @sampleRate, @beatsPerCycle = bpm, sampleRate, beatsPerCycle
    @link = link.create bpm
    @sessionState = link.create_session_state!
    @subscribers = {}
    @running = false
    @notifyCoroutine = nil
    @latency = 0.2
    @sendf = sendOSC

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
    -- @notifyCoroutine = coroutine.create ->
    @notifyCoroutine = ->
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
        -- coroutine.yield!

      @linkEnabled = false

return { :Clock }
