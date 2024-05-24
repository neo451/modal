local RL = require("readline")
local M = require("modal")
local clock = M.DefaultClock

local keywords = {}
for i, _ in pairs(M) do
   table.insert(keywords, i)
end

RL.set_complete_list(keywords)

clock:start()

local eval = function(a)
   if a then
      local res, ok = M.maxi("(" .. a .. ")", M)
      if ok then
         print(res)
      else
         print("Compilation error: " .. res)
      end
   end
end

local line

while true do
   coroutine.resume(clock.notifyCoroutine)
   line = RL.readline("modal> ")
   if not line then
      break
   end
   eval(line)
end
