local RL = require("readline")
local M = require("modal")

local keywords = {}
for i, _ in pairs(M) do
   table.insert(keywords, i)
end

RL.set_complete_list(keywords)

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

local line

while true do
   line = RL.readline("modal> ")
   if not line then
      break
   end
   eval(line)
end
