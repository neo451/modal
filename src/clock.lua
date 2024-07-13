local has_socket, socket = pcall(require, "socket")
local has_al, al = pcall(require, "abletonlink")
local losc = require "losc"()
_G.struct = nil
local types = require "types"
local Stream = types.Stream
local uv = require "luv"

local floor = math.floor
local type = type
local pairs = pairs

local sleep = function(sec)
   return os.execute("sleep " .. sec)
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

local Timetag = losc.Timetag
local Pattern = losc.Pattern
local Packet = losc.Packet

local M = {}
M.__index = M
--- Fractional precision for bundle scheduling.
-- 1000 is milliseconds. 1000000 is microseconds etc. Any precision is valid
-- that makes sense for the plugin's scheduling function.
M.precision = 1000

--- Create a new instance.
-- @tparam[options] table options Options.
-- @usage local udp = plugin.new()
-- @usage
-- local udp = plugin.new {
--   sendAddr = '127.0.0.1',
--   sendPort = 9000,
--   recvAddr = '127.0.0.1',
--   recvPort = 8000,
--   ignore_late = true, -- ignore late bundles
-- }
function M.new(options)
   local self = setmetatable({}, M)
   self.options = options or {}
   self.handle = uv.new_udp "inet"
   assert(self.handle, "Could not create UDP handle.")
   return self
end

--- Create a Timetag with the current time.
-- Precision is in milliseconds.
-- @return Timetag object with current time.
function M:now() -- luacheck: ignore
   local s, m = uv.gettimeofday()
   return Timetag.new(s, m / M.precision)
end

--- Schedule a OSC method for dispatch.
--
-- @tparam number timestamp When to schedule the bundle.
-- @tparam function handler The OSC handler to call.
function M:schedule(timestamp, handler) -- luacheck: ignore
   timestamp = math.max(0, timestamp)
   if timestamp > 0 then
      local timer = uv.new_timer()
      timer:start(timestamp, 0, handler)
   else
      handler()
   end
end

--- Start UDP server.
-- This function is blocking.
-- @tparam string host IP address (e.g. '127.0.0.1').
-- @tparam number port The port to listen on.
function M:open(host, port)
   host = host or self.options.recvAddr
   port = port or self.options.recvPort
   self.handle:bind(host, port, { reuseaddr = true })
   self.handle:recv_start(function(err, data, addr)
      assert(not err, err)
      if data then
         self.remote_info = addr
         local ok, errormsg = pcall(Pattern.dispatch, data, self)
         if not ok then
            print(errormsg)
         end
      end
   end)
   -- updated if port 0 is passed in as default (chooses a random port)
   self.options.recvPort = self.handle:getsockname().port
end

function M:run_non_blocking()
   print "listening"
   -- Run the event loop once and return
   uv.run "nowait"
end

--- Close UDP server.
function M:close()
   self.handle:recv_stop()
   if not self.handle:is_closing() then
      self.handle:close()
   end
   uv.walk(uv.close)
end

--- Send a OSC packet.
-- @tparam table packet The packet to send.
-- @tparam[opt] string address The IP address to send to.
-- @tparam[opt] number port The port to send to.
function M:send(packet, address, port)
   address = address or self.options.sendAddr
   port = port or self.options.sendPort
   packet = assert(Packet.pack(packet))
   self.handle:try_send(packet, address, port)
end

local osc, sendOSC
local udp = M.new {
   recvAddr = "127.0.0.1",
   recvPort = 9001,
   sendPort = target.port,
   sendAddr = target.address,
   -- ignore_late = true, -- ignore late bundles
}
osc = losc.new { plugin = udp }

sendOSC = function(value, ts)
   local msg = {}
   for key, val in pairs(value) do
      msg[#msg + 1] = key
      msg[#msg + 1] = val
   end
   msg.types = typesString(msg)
   msg.address = "/dirt/play"
   local b = osc.new_message(msg)
   -- local b = osc.new_bundle(ts, osc.new_message(msg))
   osc:send(b)
end
sendOSC { 1, 2, "sda" }

osc:add_handler("/ctrl", function(data)
   print(ut.dump(data))
end)

osc:add_handler("/param/{x,y,z}", function(data)
   print(ut.dump(data))
end)

local mt = { __class = "clock" }

function mt:start()
   if not self.running then
      self.running = true
      osc:open() -- ???
      return self:createNotifyCoroutine()
   end
end

function mt:stop()
   self.running = false
   print "Clock: stopped"
end

function mt:subscribe(key, pattern)
   if not self.subscribers[key] then
      self.subscribers[key] = Stream(self.callback)
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
         uv.run "nowait"
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

function Clock(bpm, sampleRate, beatsPerCycle, callback)
   bpm = bpm or 120
   sampleRate = sampleRate or (1 / 20)
   beatsPerCycle = beatsPerCycle or 4
   callback = callback or sendOSC
   return setmetatable({
      callback = callback,
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
