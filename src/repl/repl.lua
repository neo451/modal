local socket = require("socket")
print("modal repl")
local host = arg[2] or "localhost"
local port = arg[1] or 9000
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
      local res, ok = M.eval("(" .. a .. ")", M)
      if ok then
         print(res)
      else
         print("Compilation error: " .. res)
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
