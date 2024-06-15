local Stream = require "modal.stream"
local Clock = require "modal.clock"

local Streams = {}
local DefaultClock = Clock()

local p = function(pattern, key)
   if not Streams[key] then
      local stream = Stream(DefaultClock.sendf)
      DefaultClock:subscribe(stream)
      Streams[key] = stream
   end
   Streams[key].pattern = pattern
   return pattern
end

local hush = function()
   for i, v in pairs(Streams) do
      DefaultClock:unsubscribe(v)
      Streams[i] = nil
   end
end

-- stylua: ignore start
local d1 = function(a) return p(a:orbit(1), 1) end
local d2 = function(a) return p(a:orbit(2), 2) end
local d3 = function(a) return p(a:orbit(3), 3) end
local d4 = function(a) return p(a:orbit(4), 4) end
local d5 = function(a) return p(a:orbit(5), 5) end
local d6 = function(a) return p(a:orbit(6), 6) end
local d7 = function(a) return p(a:orbit(7), 7) end
local d8 = function(a) return p(a:orbit(8), 8) end

-- stylua: ignore end
return {
   p = p,
   hush = hush,
   DefaultClock = DefaultClock,
   d1 = d1,
   d2 = d2,
   d3 = d3,
   d4 = d4,
   d5 = d5,
   d6 = d6,
   d7 = d7,
   d8 = d8,
}
