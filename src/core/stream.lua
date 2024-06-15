local mt = { __class = "stream" }
local ut = require "modal.utils"

function mt:notifyTick(cycleFrom, cycleTo, s, cps, bpc, mill, now)
   print(string.format("cycleFrom : %d;  cycleTo : %d", cycleFrom, cycleTo))
   if not self.pattern then
      return
   end
   local events = self.pattern:onsetsOnly()(cycleFrom, cycleTo)
   for i = 1, #events do
      local event = events[i]
      local cycleOn = event.whole._begin
      local cycleOff = event.whole._end
      local linkOn = s:time_at_beat(cycleOn:asFloat() * bpc, 0)
      local linkOff = s:time_at_beat(cycleOff:asFloat() * bpc, 0)
      local deltaSeconds = (linkOff - linkOn) / mill
      -- local linkSecs = now / mill
      local value = event.value
      value.cps = event.value.cps or cps
      value.cycle = cycleOn:asFloat()
      value.delta = deltaSeconds
      print(ut.dump(value))
      self.sendf(value)
   end
end
mt.__index = mt

function Stream(sendf)
   local new_obj = setmetatable({}, mt)
   new_obj.isPlaying = false
   new_obj.latency = 0.3
   new_obj.pattern = nil
   new_obj.sendf = sendf
   return new_obj
end

return Stream
