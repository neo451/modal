import C from require "xi.pattern"
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

d1 = (a) -> p 1, a .. C.orbit"1"
d2 = (a) -> p 2, a .. C.orbit"2"
d3 = (a) -> p 3, a .. C.orbit"3"
d4 = (a) -> p 4, a .. C.orbit"4"
d5 = (a) -> p 5, a .. C.orbit"5"
d6 = (a) -> p 6, a .. C.orbit"6"
d7 = (a) -> p 7, a .. C.orbit"7"
d8 = (a) -> p 8, a .. C.orbit"8"

-- TODO: not working, need to go deeper
-- setcps = (cps) ->
--   DefaultClock.bpm = cps * DefaultClock.beatsPerCycle * 60
--   return DefaultClock

return {
  :p,
  :hush,
  :DefaultClock
  :d1, :d2, :d3, :d4, :d5, :d6, :d7, :d8
  :setcps
}
