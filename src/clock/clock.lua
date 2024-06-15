local socket = require "socket"
local link = require "abletonlink"
local losc = require "losc"
local plugin = require "losc.plugins.udp-socket"
local bundle = require "losc.bundle"

local floor = math.floor

local sleep = function(sec)
   return socket.sleep(sec)
end

local target = {
   name = "SuperDirt",
   address = "127.0.0.1",
   port = 57120,
   latency = 0.2,
   handshake = true,
}

local typeMap = { table = "b", number = "f", string = "s" }
local typesString = function(msg)
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

local osc = losc.new {
   plugin = plugin.new {
      sendPort = target.port,
      sendAddr = target.address,
   },
}

local sendOSC = function(value, ts)
   local msg = {}
   for key, val in pairs(value) do
      msg[#msg + 1] = key
      msg[#msg + 1] = val
   end
   msg.types = typesString(msg)
   msg.address = "/dirt/play"
   local b = osc.new_message(msg)
   return osc:send(b)
end

local mt = { __class = "clock" }

function mt:start()
   if not self.running then
      self.running = true
      return self:createNotifyCoroutine()
   end
end

-- function mt:stop()
--    self.running = false
--    return print "clock stopped"
-- end

function mt:subscribe(subscriber)
   return table.insert(self.subscribers, subscriber)
end

function mt:unsubscribe(subscriber)
   local position = nil
   for i, sub in ipairs(self.subscribers) do
      if sub == subscriber then
         position = i
      end
   end
   if position ~= nil then
      return table.remove(self.subscribers, position)
   end
end

function mt:createNotifyCoroutine()
   self.notifyCoroutine = coroutine.create(function(f)
      local start = self.link:clock_micros()
      local ticks = 0
      local mill = 1000000
      local frame = self.sampleRate * mill
      while self.running do
         ticks = ticks + 1
         local logicalNow = floor(start + (ticks * frame))
         local logicalNext = floor(start + ((ticks + 1) * frame))
         local now = self.link:clock_micros()
         local wait = (logicalNow - now) / mill
         if wait > 0 then
            sleep(wait)
         end
         if not self.running then
            break
         end
         self.link:capture_audio_session_state(self.sessionState)
         local cps = (self.sessionState:tempo() / self.beatsPerCycle) / 60
         local cycleFrom = self.sessionState:beat_at_time(logicalNow, 0) / self.beatsPerCycle
         local cycleTo = self.sessionState:beat_at_time(logicalNext, 0) / self.beatsPerCycle
         -- print(string.format("cycleFrom : %d;  cycleTo : %d", cycleFrom, cycleTo))
         f()
         for _, sub in ipairs(self.subscribers) do
            sub:notifyTick(cycleFrom, cycleTo, self.sessionState, cps, self.beatsPerCycle, mill, now)
         end
         coroutine.yield()
      end
      self.linkEnabled = false
   end)
end

mt.__index = mt

local function Clock(bpm, sampleRate, beatsPerCycle)
   bpm = bpm or 120
   sampleRate = sampleRate or (1 / 20)
   beatsPerCycle = beatsPerCycle or 4
   local new_obj = setmetatable({}, mt)
   new_obj.bpm, new_obj.sampleRate, new_obj.beatsPerCycle = bpm, sampleRate, beatsPerCycle
   new_obj.link = link.create(bpm)
   new_obj.sessionState = link.create_session_state()
   new_obj.subscribers = {}
   new_obj.running = false
   new_obj.notifyCoroutine = nil
   new_obj.latency = 0.2
   new_obj.sendf = sendOSC
   return new_obj
end

return Clock
