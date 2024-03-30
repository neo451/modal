local Clock
Clock = require("modal.clock").Clock
require("moon.all")
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
      local events = self.pattern:onsetsOnly():querySpan(cycleFrom, cycleTo)
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
        self:sendf(value)
      end
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, sendf)
      self.isPlaying = false
      self.latency = 0.3
      self.pattern = nil
      self.sendf = sendf
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
