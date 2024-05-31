local socket = require("socket")
local inspect = require"inspect"
print("modal repl")
local host = "localhost"
-- local port = arg[1] or 9000
local port = 9000
local RL = require("readline")
local M = require("modal")

local keywords = {}
for i, _ in pairs(M) do
   table.insert(keywords, i)
end

RL.set_complete_list(keywords)

local line

local ok, c = pcall(socket.connect, host, port)

local eval = function(a)
   if a then
      local res, ok = M.eval(M)(a)
      if ok then
         if res then
            io.write(M.dump(res))
         end
      else
         print("Compilation error: " .. res)
      end
   end
end

if arg[1] == "og" then
   eval = function(a)
      local ok, res = pcall(loadstring, "return " .. a)
      if ok then
         setfenv(res, M)
         print(res())
      else
         print("lua error" .. res())
      end
   end
end

while true do
   -- c = assert(socket.connect(host, port))
   line = RL.readline("modal> ")
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
