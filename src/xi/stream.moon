losc = require "losc"
plugin = require "losc.plugins.udp-socket"

StreamTarget = { name: "SuperDirt", address: "127.0.0.1", port: 57120, latency: 0.2, handshake: true }

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

class Stream
  new: (target = StreamTarget)=>
    target = target
    osc = losc\new({
      plugin: plugin\new({
        sendPort: target.port
        sendAddr: target.address 
      })})
    isPlaying = false
    latency = 0.2
    pattern = nil

  type: -> "stream"
