local socket = require "socket"
print "modal repl   :? for help"
local host = "localhost"
local port = 9000
local RL = require "readline"
local M = require "modal"
local maxi = require "modal.maxi"

local keywords = {}
for i, _ in pairs(M) do
   table.insert(keywords, i)
end

RL.set_complete_list(keywords)

local line

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

local eval = function(a)
   local evalf = maxi(M, true)
   -- if a == ">>lua" then
   --    evalf = function(a)
   --       return pcall(loadstring, "return " .. a)()
   --    end
   --    return
   -- elseif a == ">>modal" then
   --    evalf = maxi(M, true)
   -- end
   if a then
      if a:sub(1, 1) == ":" then
         -- print(a:sub(1, #a))
         local name, param = string.match(a, "(%a+)%s(%a*)")
         name = name and name or a:sub(2, #a)
         param = param and param or nil
         print(optf[name](param))
         return
      end
      local res, ok = evalf(a)
      if ok then
         if res then
            io.write(M.dump(res))
         end
      else
         print("Compilation error: " .. res)
      end
   end
end

-- if arg[1] == "og" then
--    eval = function(a)
--       local ok, res = pcall(loadstring, "return " .. a)
--       if ok then
--          setfenv(res, M)
--          print(res())
--       else
--          print("lua error" .. res())
--       end
--    end
-- end

while true do
   -- c = assert(socket.connect(host, port))
   line = RL.readline "modal> "
   if not line then
      break
   end
   if line == "exit" then
      break
   end
   print(eval(line))
   if c then
      c:send(line .. "\n")
   end
end

c:close()
