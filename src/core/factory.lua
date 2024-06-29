local Clock = require "modal.clock"
local DefaultClock = Clock()
local factory = {}

function factory.p(key, pattern)
   DefaultClock:subscribe(key, pattern)
   return pattern
end

-- TODO: cause server to freeze ...
function factory._p(key)
   DefaultClock:unsubscribe(key)
end

factory.p_ = factory._p

function factory.hush()
   for i, _ in pairs(DefaultClock.subscribers) do
      DefaultClock:unsubscribe(i)
   end
end

-- function M.panic()
--    M.hush()
--    once(s "superpanic")
-- end
-- panic :: Tidally => IO ()
-- panic = hush >> once (sound "superpanic")

for i = 1, 16 do
   if i <= 12 then
      factory["d" .. i] = function(a)
         return factory.p(i, a:orbit(i - 1))
      end
   else
      factory["d" .. i] = function(a)
         return factory.p(i, a)
      end
   end
   factory["_d" .. i] = function()
      return factory._p(i)
   end
   factory["d" .. i .. "_"] = function()
      return factory._p(i)
   end
end

factory.DefaultClock = DefaultClock

function factory.setcps(cps)
   DefaultClock:setcps(cps)
end

function factory.setbpm(bpm)
   DefaultClock:setbpm(bpm)
end

factory.bpm = factory.setbpm
factory.cps = factory.setcps

return factory
