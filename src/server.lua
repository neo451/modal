local function server(port)
   local uv = require "luv" or vim.uv
   -- local socket = require "socket"
   local ut = require "modal.utils"
   local modal = require "modal"
   local notation = require "modal.notation"
   local maxi = notation.maxi(modal)
   local log = ut.log

   local clock = modal.DefaultClock
   clock:start()

   local eval = function(a)
      local ok, fn = pcall(maxi, a)
      if not ok then
         log.warn("syntax error: " .. fn)
      else
         print(fn)
      end
   end

   port = port or 9000
   -- local sock = assert(socket.bind(host, port))
   -- local i, p = sock:getsockname()
   -- assert(i, p)
   --
   -- print("Waiting connection from repl on " .. i .. ":" .. p .. "...")
   -- local c = assert(sock:accept())
   -- c:settimeout(0)
   local tcp = uv.new_tcp()
   tcp:bind("127.0.0.1", 9000)
   tcp:listen(128, function(err)
      assert(not err, err)
      local client = uv.new_tcp()
      tcp:accept(client)
      client:read_start(function(err, chunk)
         assert(not err, err)
         if chunk then
            eval(chunk)
         else
            client:shutdown()
            client:close()
         end
      end)
   end)

   -- local l, e

   -- local listen = function()
   --    l, e = c:receive()
   --    if not e then
   --       eval(l)
   --    end
   -- end
   --
   local timer = uv.new_timer()
   timer:start(0, 10, function()
      coroutine.resume(clock.co)
   end)
   uv.run()
end

modal.server = server

return server
