local socket = require "socket"
print "modal repl   :? for help"
local host = "localhost"
local port = 9000
local RL = require "readline"
local M = require "modal"
local ut = require "modal.utils"
local maxi = require "modal.maxi"

local keywords = {}
for i, _ in pairs(M) do
   table.insert(keywords, i)
end

RL.set_complete_list(keywords)

local line

local function show_sig(t)
   local function format(a)
      if type(a[1]) == "table" then
         return string.format("(%s)", show_sig(a))
      elseif a.constructor then
         return string.format("%s %s", a.constructor, a[1])
      elseif type(a) == "string" then
         return a
      end
   end
   local s = ""
   for i = 1, #t do
      s = s .. format(t[i]) .. " -> "
   end
   return s .. format(t.ret)
end

local optf = {
   ["?"] = function()
      return [[
:v  show _VERSION
:t  get type for lib func (TODO: for expression)
:q  quit repl ]]
   end,
   t = function(a)
      return show_sig(M.t[a])
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
         -- print(a:sub(1, #a))
         local name, param = string.match(a, "(%a+)%s(%a*)")
         name = name and name or a:sub(2, #a)
         param = param and param or nil
         print(optf[name](param))
         return
      end
      local fn, ok = evalf(a)
      if ok then
         local fok, res = pcall(fn)
         if fok then
            io.write(M.dump(res))
         end
      else
         print("Compilation error: " .. res)
      end
   end
end

RL.set_options { keeplines = 1000, histfile = "~/.synopsis_history" }
RL.set_readline_name "modal"

while true do
   -- c = assert(socket.connect(host, port))
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
end

RL.save_history()
c:close()
os.exit()
