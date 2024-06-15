local socket = require "socket"
local al = require "abletonlink"
local losc = require "losc"
local plugin = require "losc.plugins.udp-socket"
local timetag = require "losc.timetag"
local Stream = require "modal.stream"
local ut = require "modal.utils"

local floor = math.floor
local tremove = table.remove

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
   print(ut.dump(value), ts)
   -- local b = osc.new_bundle(timetag.new(ts), osc.new_message(msg))
   local b = osc.new_bundle(ts, osc.new_message(msg))
   osc:send(b)
end

local mt = { __class = "clock" }

function mt:start()
   print "Clock: started"
   if not self.running then
      self.running = true
      print("running " .. tostring(self.running))
      return self:createNotifyCoroutine()
   end
end

function mt:stop()
   self.running = false
   print "Clock: stopped"
end

function mt:subscribe(key, pattern)
   if not self.subscribers[key] then
      self.subscribers[key] = Stream(sendOSC)
   end
   self.subscribers[key].pattern = pattern
end

function mt:unsubscribe(key)
   tremove(self.subscribers, key)
   -- self.subscribers[key] = nil
end

function mt:createNotifyCoroutine()
   self.co = coroutine.create(function(f)
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
         if f then
            f()
         end
         for _, sub in pairs(self.subscribers) do
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
   return setmetatable({
      bpm = bpm,
      sampleRate = sampleRate,
      beatsPerCycle = beatsPerCycle,
      link = al.create(bpm),
      sessionState = al.create_session_state(),
      subscribers = {},
      running = false,
      latency = 0.2,
   }, mt)
end

return Clock
