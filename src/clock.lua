local has_socket, socket = pcall(require, "socket")
local has_al, al = pcall(require, "abletonlink")
local has_losc, losc = pcall(require, "losc")
local has_plugin, plugin = pcall(require, "losc.plugins.libuv")
_G.struct = nil
local types = require "types"
local Stream = types.Stream

local floor = math.floor
local type = type
local pairs = pairs

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
   local ts = ""
   for i = 1, #msg do
      local x = msg[i]
      if typeMap[type(x)] then
         ts = ts .. typeMap[type(x)]
      else
         ts = ts .. "b"
      end
   end
   return ts
end

local osc, sendOSC
if has_losc then
   osc = losc.new {
      plugin = plugin.new {
         sendPort = target.port,
         sendAddr = target.address,
      },
   }
   sendOSC = function(value, ts)
      local msg = {}
      for key, val in pairs(value) do
         msg[#msg + 1] = key
         msg[#msg + 1] = val
      end
      msg.types = typesString(msg)
      msg.address = "/dirt/play"
      -- local b = osc.new_message(msg)
      local b = osc.new_bundle(ts, osc.new_message(msg))
      osc:send(b)
   end
end

local mt = { __class = "clock" }

function mt:start()
   if not self.running then
      self.running = true
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
   self.subscribers[key] = nil
end

function mt:setbpm(bpm)
   self.sessionState:set_tempo(bpm, 0)
   self.link:commit_audio_session_state(self.sessionState)
end

function mt:setcps(cps)
   self.sessionState:set_tempo(cps * self.beatsPerCycle * 60, 0)
   self.link:commit_audio_session_state(self.sessionState)
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

function Clock(bpm, sampleRate, beatsPerCycle)
   bpm = bpm or 120
   sampleRate = sampleRate or (1 / 20)
   beatsPerCycle = beatsPerCycle or 4
   return setmetatable({
      bpm = bpm,
      sampleRate = sampleRate,
      beatsPerCycle = beatsPerCycle,
      link = has_al and al.create(bpm) or {}, -- HACK:
      sessionState = has_al and al.create_session_state() or {},
      subscribers = {},
      running = false,
      latency = 0.2,
   }, mt)
end

return Clock
