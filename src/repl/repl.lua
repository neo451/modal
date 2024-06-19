local socket = require "socket"
print "modal repl   :? for help"
local host = "localhost"
local port = 9000
local RL = require "readline"
local M = require "modal"
local maxi = require "modal.maxi"

local loadstring = loadstring or load

local keywords = {}
for i, _ in pairs(M) do
   keywords[#keywords + 1] = i
end

RL.set_complete_list(keywords)

local optf = {
   ["?"] = function()
      return [[
:v  show _VERSION
:t  get type for lib func (TODO: for expression)
:q  quit repl ]]
   end,
   t = function(a)
      return M.t[a]
   end,
   v = function()
      return M._VERSION
   end,
   q = function()
      os.exit()
   end,
}

local ok, c = pcall(socket.connect, host, port)

M()

local evalf = maxi(_G, true)
local eval = function(a)
   if a then
      if a:sub(1, 1) == ":" then
         local name, param = a:match "(%a+)%s(%a*)"
         name = name and name or a:sub(2, #a)
         param = param and param or nil
         print(optf[name](param))
         return
      else
         local fn = evalf(a)
         if fn then
            local fok, res = pcall(fn)
            if fok and res then
               io.write(tostring(res))
            end
         else
            io.write(fn)
         end
      end
   end
end

RL.set_options { keeplines = 1000, histfile = "~/.synopsis_history" }
RL.set_readline_name "modal"

local line
while true do
   line = RL.readline "modal> "
   if not line then
      break
   end
   RL.add_history(line)
   if line == "exit" then
      break
   end

   if line == ">>lua" then
      eval = function(x)
         return loadstring(x)()
      end
   elseif line == ">>modal" then
      eval = maxi(_G, true)
   elseif line ~= "" then
      print(eval(line))
      if c then
         c:send(line .. "\n")
      end
   end
   RL.save_history()
end

RL.save_history()
c:close()
os.exit()
