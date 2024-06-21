local socket = require "socket"
local maxi = require "modal.maxi"
local log = require "modal.log"
local M = require "modal"
local ut = require "modal.utils"

local clock = M.DefaultClock
clock:start()

local host = "*"
local port = arg[1] or 9000
local sock = assert(socket.bind(host, port))
local i, p = sock:getsockname()
assert(i, p)

print("Waiting connection from repl on " .. i .. ":" .. p .. "...")
local c = assert(sock:accept())
c:settimeout(0)

print "Connected"

M(true)

local evalf = maxi(_G, true)

local eval = function(a)
   local ok, fn = pcall(evalf, a)
   if not ok then
      log.warn("syntax error: " .. fn)
   end
   local ok, res = pcall(fn)
   if ok then
      if res then
         print(res)
      end
   else
      log.warn("function error: " .. res)
   end
end

local l, e

local listen = function()
   l, e = c:receive()
   if not e then
      eval(l)
   end
end

repeat
   coroutine.resume(clock.co, listen)
until false
