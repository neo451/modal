local Stream = require "modal.stream"
local Clock = require "modal.clock"
local DefaultClock = Clock()
local M = {}

function M.p(pattern, key)
   if not DefaultClock.subscribers[key] then
      local stream = Stream(DefaultClock.sendf)
      DefaultClock:subscribe(key, stream)
   end
   DefaultClock.subscribers[key].pattern = pattern
   return pattern
end

function M.hush()
   for i, _ in pairs(DefaultClock.subscribers) do
      DefaultClock:unsubscribe(i)
   end
end

for i = 1, 16 do
   if i <= 12 then
      M["d" .. i] = function(a)
         return M.p(a:orbit(i - 1), i)
      end
   else
      M["d" .. i] = function(a)
         return M.p(a, i)
      end
   end
end

M.DefaultClock = DefaultClock

return M
