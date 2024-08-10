local function repl()
   local uv = require "luv" or vim.uv
   local client = uv.new_tcp()
   local host = "127.0.0.1"
   local port = 9000
   local connected = false
   uv.tcp_connect(client, host, port, function(err)
      print "connecting"
      print(err)
      if err == "" then
         connected = true
      end
   end)

   RL = require "readline"
   local has_RL, RL = pcall(require, "readline")
   -- local modal = require "modal"
   -- local notation = require("modal").notation
   local maxi = notation.maxi(modal)

   local keywords = {}
   for i, _ in pairs(modal) do
      keywords[#keywords + 1] = i
   end

   if has_RL then
      RL.set_complete_list(keywords)
      RL.set_options { keeplines = 1000, histfile = "~/.synopsis_history" }
      RL.set_readline_name "modal"
   end

   local optf = {
      ["?"] = function()
         return [[
:v  show _VERSION
:t  get type for lib func (TODO: for expression)
:q  quit repl ]]
      end,
      t = function(a)
         return tostring(modal.t[a])
      end,
      v = function()
         return modal._VERSION
      end,
      -- info = function(name)
      --    return dump(doc[name])
      -- end,
      q = function()
         if client then
            client:close()
         end
         os.exit()
      end,
   }

   -- TODO: see luaish, first run as lua with multiline? no ambiguiaty?>
   local eval = function(a)
      if a:sub(1, 1) == ":" then
         local name, param = a:match "(%a+)%s(%a*)"
         name = name and name or a:sub(2, #a)
         param = param and param or nil
         return optf[name](param)
      else
         local fn = modal.ut.dump(maxi(a))
         return fn
      end
   end

   local function readline(a)
      io.write(a)
      return io.read()
   end

   local read = has_RL and RL.readline or readline

   local line
   print "modal repl   :? for help"
   while true do
      line = read "> "
      if line ~= "" then
         local res = eval(line)
         if res then
            print(res)
         end
         if has_RL then
            RL.add_history(line)
            -- RL.save_history()
         end
         if connected then
            print "connected"
            uv.write(client, line .. "\n")
            uv.run "once"
         end
      end
   end
end
modal.repl = repl

return repl
