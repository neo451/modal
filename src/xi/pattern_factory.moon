import Pattern from require "xi.pattern"
import Stream from require "xi.stream"
import Clock from require "xi.clock"

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

return { :p, :hush, :DefaultClock }
