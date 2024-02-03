local losc = require("losc")
local plugin = require("losc.plugins.udp-socket")
local bundle = require("losc.bundle")
local Fraction
Fraction = require("xi.fraction").Fraction
local dump
dump = require("xi.utils").dump
StreamTarget = {
  name = "SuperDirt",
  address = "127.0.0.1",
  port = 57120,
  latency = 0.2,
  handshake = true
}
local typeMap = {
  table = "b",
  number = "f",
  string = "s"
}
local GenerateTypesString
GenerateTypesString = function(msg)
  local types = ""
  for _index_0 = 1, #msg do
    local x = msg[_index_0]
    if typeMap[type(x)] then
      types = types .. typeMap[type(x)]
    else
      types = types .. "b"
    end
  end
  return types
end
local Stream
do
  local _class_0
  local _base_0 = {
    type = function()
      return "stream"
    end,
    notifyTick = function(self, cycleFrom, cycleTo, s, cps, bpc, mill, now)
      if not self.pattern then
        return 
      end
      local events = self.pattern:onsetsOnly():querySpan(Fraction(cycleFrom), Fraction(cycleTo))
      for _index_0 = 1, #events do
        local event = events[_index_0]
        local cycleOn = event.whole._begin
        local cycleOff = event.whole._end
        local linkOn = s:time_at_beat(cycleOn:asFloat() * bpc, 0)
        local linkOff = s:time_at_beat(cycleOff:asFloat() * bpc, 0)
        local deltaSeconds = (linkOff - linkOn) / mill
        local linkSecs = now / mill
        local value = event.value
        value.cps = event.value.cps or cps
        value.cycle = cycleOn:asFloat()
        value.delta = deltaSeconds
        local msg = { }
        for key, val in pairs(value) do
          table.insert(msg, key)
          table.insert(msg, val)
        end
        msg.types = GenerateTypesString(msg)
        msg.address = "/dirt/play"
        local b = self.osc.new_message(msg)
        self.osc:send(b)
      end
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, target)
      if target == nil then
        target = StreamTarget
      end
      self.target = target
      self.osc = losc.new({
        plugin = plugin.new({
          sendPort = target.port,
          sendAddr = target.address
        })
      })
      self.isPlaying = false
      self.latency = 0.3
      self.pattern = nil
    end,
    __base = _base_0,
    __name = "Stream"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  Stream = _class_0
end
return {
  Stream = Stream
}
