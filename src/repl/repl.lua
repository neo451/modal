local socket = require "socket"
print "modal repl   :? for help"
local host = "localhost"
local port = 9000
local RL = require "readline"
local M = require "modal"
local maxi = require("modal.maxi").maxi(M)
local ut = require "modal.utils"

local keywords = {}
for i, _ in pairs(M) do
   keywords[#keywords + 1] = i
end

RL.set_complete_list(keywords)

local ok, c = pcall(socket.connect, host, port)

M()

local optf = {
   ["?"] = function()
      return [[
:v  show _VERSION
:t  get type for lib func (TODO: for expression)
:q  quit repl ]]
   end,
   t = function(a)
      return tostring(M.t[a])
   end,
   v = function()
      return M._VERSION
   end,
   -- info = function(name)
   --    return dump(doc[name])
   -- end,
   q = function()
      if c then
         c:close()
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
      local fn = maxi(a)
      return fn
   end
end

RL.set_options { keeplines = 1000, histfile = "~/.synopsis_history" }
RL.set_readline_name "modal"

local line
while true do
   line = RL.readline "> "
   if line == "exit" then
      if c then
         c:close()
      end
      break
   end

   if line ~= "" then
      local res = eval(line)
      if res then
         print(res)
      end
      RL.add_history(line)
      RL.save_history()
      if c then
         c:send(line .. "\n")
      end
   end
end

c:close()
os.exit()
