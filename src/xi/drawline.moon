--- drawline.moon
-- @module drawline

import filter, reduce, flatten, dump, dumpval, type from require "xi.utils"
import Fraction, gcd_reduce from require "xi.fraction"
import Pattern, reify from require "xi.pattern"
import sound from require "xi.control"
--- intended for debugging, drawline renders the pattern as a string, where each character represents the same time span.
-- should only be used with single characters as values, otherwise the character slots will be messed up.
-- character legend:
-- - "|" cycle separator
-- - "-" hold previous value
-- - "." silence
-- @tparam Pattern pattern to draw
-- @tparam number how many char to draw
-- @treturn string result string
-- @usage
-- print drawLine slowcat pure(1)\_fast(4), 2

map = (func, items) -> [ func(v, i) for i, v in ipairs items ]

drawline = (pat, chars) ->
  pat = reify pat
  chars = chars or 60
  cycle = 0
  pos = Fraction(0)
  lines = { "" }
  emptyLine = ""
  while #lines[1] < chars do
    events = pat\querySpan cycle, cycle + 1
    filterfunc = (event) -> event\hasOnset!
    events_with_onset = filter filterfunc, events
    mapfunc = (event) -> event\duration!
    durations = map mapfunc, events_with_onset
    charFraction = gcd_reduce durations
    totalSlots = Fraction(1) / charFraction
    lines = map ((line) -> line .. "|"), lines
    emptyLine = emptyLine .. "|"
    for _ = 1, totalSlots\asFloat() do
      _begin, _end = pos, pos + charFraction
      filterfunc = (event) -> event.whole._begin <= _begin and event.whole._end >= _end
      matches = filter filterfunc, events
      missingLines = #matches - #lines
      if missingLines > 0 then
        for i = 1, missingLines
          lines ..= missingLines
      mapfunc = (line, index) -> 
        event = matches[index]
        if event ~= nil
          isOnset = event.whole._begin == _begin
          char = nil
          if isOnset
            char = dumpval event.value
          else
            char = "-"
          -- char = isOnset and dumpval event.value or "-"
          return line .. char
        return line .. "."
      lines = map mapfunc, lines
      emptyLine = emptyLine .. "."
      pos = pos + charFraction
    cycle = cycle + 1
  return table.concat lines

return { :drawline }
