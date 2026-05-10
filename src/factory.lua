local factory = {}

local _default_clock
local function default_clock()
   if not _default_clock then
      _default_clock = Clock()
   end
   return _default_clock
end

factory.default_clock = default_clock

function factory.p(key, pattern)
   default_clock():subscribe(key, pattern)
   return pattern
end

-- TODO: cause server to freeze ...
function factory._p(key)
   default_clock():unsubscribe(key)
end

factory.p_ = factory._p

function factory.hush()
   if not _default_clock then
      return
   end
   local keys = {}
   for k in pairs(_default_clock.subscribers) do
      keys[#keys + 1] = k
   end
   for _, k in ipairs(keys) do
      _default_clock:unsubscribe(k)
   end
end

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

function factory.setcps(cps)
   default_clock():setcps(cps)
end

function factory.setbpm(bpm)
   default_clock():setbpm(bpm)
end

factory.bpm = factory.setbpm
factory.cps = factory.setcps

return factory
