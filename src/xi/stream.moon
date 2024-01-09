losc = require "losc"
plugin = require "losc.plugins.udp-socket"

export StreamTarget = { name: "SuperDirt", address: "127.0.0.1", port: 57120, latency: 0.2, handshake: true }

typeMap = {
  table: "b",
  number: "f",
  string: "s",
}

GenerateTypesString = (msg) ->
  types = ""
  for x in *msg do
    if typeMap[type(x)] then
      types = types .. typeMap[type(x)]
    else
      types = types .. "b"
  return types

export class Stream
  new:(target = StreamTarget) =>
    @target = target
    @osc = losc.new({
      plugin: plugin.new({
        sendPort: target.port
        sendAddr: target.address 
      })})
    @isPlaying = false
    @latency = 0.2
    @pattern = nil

  type: -> "stream"

  notifyTick: (cycleFrom, cycleTo, s, cps, bpc, mill, now) =>
    if not @pattern
      return
    events = @pattern\onsetsOnly!\querySpan cycleFrom, cycleTo

    print "cycle from: ", cycleFrom, " ", "cycle to: ", cycleTo

    for event in *events
      cycleOn = event.whole._begin
      cycleOff = event.whole._end
      linkOn = s\time_at_beat cycleOn\asFloat! * bpc, 0
      linkOff = s\time_at_beat cycleOff\asFloat! * bpc, 0
      deltaSeconds = (linkOff - linkOn) / mill
      linkSecs = now / mill
      libloDiff = losc\now! + (-linkSecs)
      ts = libloDiff + @latency + (linkOn / mill)

      value = event.value
      value.cps = cps
      value.cycle = cycleOn\asFloat!
      value.delta = deltaSeconds

      msg = {}
      for key, val in pairs value do
        table.insert msg, key
        table.insert msg, val
      -- print "send", dump value
      msg.types = GenerateTypesString msg
      msg.address = "/dirt/play"

      b = @osc.new_message msg

      print dump b

      @osc\send b

