local mt = { __class = "stream" }
local losc = require "losc"

function mt:notifyTick(cycleFrom, cycleTo, s, cps, bpc, mill, now)
   if not self.pattern then
      return
   end
   local events = self.pattern:onsetsOnly()(cycleFrom, cycleTo)
   for _, ev in ipairs(events) do
      local cycleOn = ev.whole._begin
      local cycleOff = ev.whole._end
      local linkOn = s:time_at_beat(cycleOn:asFloat() * bpc, 0)
      local linkOff = s:time_at_beat(cycleOff:asFloat() * bpc, 0)
      local deltaSeconds = (linkOff - linkOn) / mill
      local value = ev.value
      value.cps = ev.value.cps or cps
      value.cycle = cycleOn:asFloat()
      value.delta = deltaSeconds
      local link_secs = now / mill
      local nudge = 0
      local diff = losc:now() + -link_secs
      -- print(link_secs)
      -- print(diff:seconds())
      local ts = diff + (linkOn / mill) + self.latency + nudge
      self.sendf(value, ts)
   end
end
mt.__index = mt

function Stream(sendf)
   return setmetatable({ latency = 0.2, sendf = sendf }, mt)
end

return Stream
