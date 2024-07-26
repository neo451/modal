local function server(port)
   local uv = require "luv" or vim.uv
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

   local tcp = uv.new_tcp()
   tcp:bind("127.0.0.1", 9000)
   tcp:listen(128, function(err)
      assert(not err, err)
      print "listening"
      local client = uv.new_tcp()
      tcp:accept(client)
      client:read_start(function(err, chunk)
         -- assert(not err, err)
         if chunk then
            print(chunk)
            eval(chunk)
         else
            client:shutdown()
            client:close()
         end
      end)
   end)

   local timer = uv.new_timer()
   timer:start(0, 10, function()
      coroutine.resume(clock.co)
   end)
   uv.run()
end

modal.server = server

return server
