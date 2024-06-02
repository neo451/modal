local socket = require "socket"

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

local eval = function(a)
   if a then
      local res, ok = M.eval("(" .. a .. ")", M)
      if ok then
         print(res)
      else
         print("Compilation error: " .. res)
      end
   end
end

local l, e

local listen = function(client)
   l, e = client:receive()
   while not e do
      -- c = assert(s:accept())
      eval(l)
      print(l)
      l, e = client:receive()
   end
end

while coroutine.resume(clock.notifyCoroutine) do
   listen(c)
end
