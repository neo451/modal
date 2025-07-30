--- TODO: quit if client detach?

local function server()
   local socket = require "socket"
   -- local ut = require "modal.utils"
   -- local modal = require "modal"
   -- local notation = require "modal.notation"
   local maxi = notation.maxi(modal)
   local log = ut.log

   local clock = modal.DefaultClock
   clock:start()

   local host = "*"
   local port = 9000
   local sock = assert(socket.bind(host, port))
   local i, p = sock:getsockname()
   assert(i, p)

   print("Waiting connection from repl on " .. i .. ":" .. p .. "...")
   local c = assert(sock:accept())
   c:settimeout(0)

   print "Connected"

   local eval = function(a)
      local ok, fn = pcall(maxi, a)
      if not ok then
         log.warn("syntax error: " .. fn)
      else
         print(fn)
      end
   end

   local l, e

   local listen = function()
      l, e = c:receive()
      if not e then
         print("recieved: ", l)
         eval(l)
      end
   end

   repeat
      coroutine.resume(clock.co, listen)
   until false
end

modal.server = server

return server
