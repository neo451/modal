local C
C = require("xi.pattern").C
local Stream
Stream = require("xi.stream").Stream
local Clock
Clock = require("xi.clock").Clock
local Streams = { }
local DefaultClock = Clock()
local p
p = function(key, pattern)
  if not Streams[key] then
    local stream = Stream()
    DefaultClock:subscribe(stream)
    Streams[key] = stream
  end
  Streams[key].pattern = pattern
  return pattern
end
local hush
hush = function()
  for _index_0 = 1, #Streams do
    local stream = Streams[_index_0]
    DefaultClock:unsubscribe(stream)
  end
  Streams = { }
end
local d1
d1 = function(a)
  return p(1, a .. C.orbit("1"))
end
local d2
d2 = function(a)
  return p(2, a .. C.orbit("2"))
end
local d3
d3 = function(a)
  return p(3, a .. C.orbit("3"))
end
local d4
d4 = function(a)
  return p(4, a .. C.orbit("4"))
end
local d5
d5 = function(a)
  return p(5, a .. C.orbit("5"))
end
local d6
d6 = function(a)
  return p(6, a .. C.orbit("6"))
end
local d7
d7 = function(a)
  return p(7, a .. C.orbit("7"))
end
local d8
d8 = function(a)
  return p(8, a .. C.orbit("8"))
end
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
  setcps = setcps
}
