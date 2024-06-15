local socket = require "socket"
local maxi = require "modal.maxi"
local M = require "modal"

local clock = M.DefaultClock
clock:start()

local host = "*"
local port = arg[1] or 9000
local s = assert(socket.bind(host, port))
local i, p = s:getsockname()
assert(i, p)
print("Waiting connection from repl on " .. i .. ":" .. p .. "...")
local c = assert(s:accept())
c:settimeout(0)

print "Connected"

M()
-- local log = require "modal.log"

local evalf = maxi(_G, true)
local eval = function(a)
   local ok, fn = pcall(evalf, a)
   if not ok then
      -- log.warn("syntax error: " .. fn)
      error("syntax error: " .. fn)
   end
   local ok, res = pcall(fn)
   if not ok then
      error("function error: " .. res)
   end
   return res
end

local l, e

local listen = function()
   l, e = c:receive()
   if not e then
      eval(l)
   end
end

while true do
   coroutine.resume(clock.notifyCoroutine, listen)
end
