local F = require "modal.fraction"
local Fraction, gcd_reduce = F.Fraction, F.gcd_reduce
local P = require "modal.pattern"
local ut = require "modal.utils"

local map = function(func, items)
   local _accum_0 = {}
   local _len_0 = 1
   for i, v in ipairs(items) do
      _accum_0[_len_0] = func(v, i)
      _len_0 = _len_0 + 1
   end
   return _accum_0
end

local drawline = function(pat, chars)
   chars = chars or 60
   local cycle = 0
   local pos = Fraction(0)
   local lines = { "" }
   local emptyLine = ""
   while #lines[1] < chars do
      local events = P.querySpan(cycle, cycle + 1, pat)
      local filterfunc = function(event)
         return event:hasOnset()
      end
      local events_with_onset = ut.filter(filterfunc, events)
      local mapfunc = function(event)
         return event:duration()
      end
      local durations = map(mapfunc, events_with_onset)
      local charFraction = gcd_reduce(durations)
      local totalSlots = Fraction(1) / charFraction
      lines = map(function(line)
         return line .. "|"
      end, lines)
      emptyLine = emptyLine .. "|"
      for _ = 1, totalSlots:asFloat() do
         local _begin, _end = pos, pos + charFraction
         filterfunc = function(event)
            return event.whole._begin <= _begin and event.whole._end >= _end
         end
         local matches = ut.filter(filterfunc, events)
         local missingLines = #matches - #lines
         if missingLines > 0 then
            for _ = 1, missingLines do
               lines = lines .. missingLines
            end
         end
         mapfunc = function(line, index)
            local event = matches[index]
            if event ~= nil then
               local isOnset = event.whole._begin == _begin
               local char = nil
               if isOnset then
                  char = dump(event.value)
               else
                  char = "-"
               end
               return line .. char
            end
            return line .. "."
         end
         lines = map(mapfunc, lines)
         emptyLine = emptyLine .. "."
         pos = pos + charFraction
      end
      cycle = cycle + 1
   end
   return table.concat(lines)
end

return drawline
