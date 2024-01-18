import Pattern from require "xi.pattern"
import Stream from require "xi.stream"
import Clock from require "xi.clock"
require "moon.all"
Streams = {}
DefaultClock = Clock!

p = (key, pattern) ->
  if not Streams[key]
    stream = Stream!
    DefaultClock\subscribe stream
    Streams[key] = stream
  Streams[key].pattern = pattern
  return pattern

hush = ->
  for stream in *Streams
    DefaultClock\unsubscribe stream
  Streams = {}

-- d1 = (a) -> p 1, bind_methods a
-- d2 = (a) -> p 2, bind_methods a
-- d3 = (a) -> p 3, bind_methods a
-- d4 = (a) -> p 4, bind_methods a
-- d5 = (a) -> p 5, bind_methods a
-- d6 = (a) -> p 6, bind_methods a
-- d7 = (a) -> p 7, bind_methods a
-- d8 = (a) -> p 8, bind_methods a

d1 = (a) -> p 1, a
d2 = (a) -> p 2, a
d3 = (a) -> p 3, a
d4 = (a) -> p 4, a
d5 = (a) -> p 5, a
d6 = (a) -> p 6, a
d7 = (a) -> p 7, a
d8 = (a) -> p 8, a

return {
  :p,
  :hush,
  :DefaultClock
  :d1, :d2, :d3, :d4, :d5, :d6, :d7, :d8
}
