local filter, reduce, flatten, dump
do
   local _obj_0 = require("modal.utils")
   filter, reduce, flatten, dump = _obj_0.filter, _obj_0.reduce, _obj_0.flatten, _obj_0.dump
end
local Fraction, gcd_reduce
do
   local _obj_0 = require("modal.fraction")
   Fraction, gcd_reduce = _obj_0.Fraction, _obj_0.gcd_reduce
end
local Pattern, reify
do
   local _obj_0 = require("modal.pattern")
   Pattern, reify = _obj_0.Pattern, _obj_0.reify
end
local map
map = function(func, items)
   local _accum_0 = {}
   local _len_0 = 1
   for i, v in ipairs(items) do
      _accum_0[_len_0] = func(v, i)
      _len_0 = _len_0 + 1
   end
   return _accum_0
end
local drawline
drawline = function(pat, chars)
   pat = reify(pat)
   chars = chars or 60
   local cycle = 0
   local pos = Fraction(0)
   local lines = {
      "",
   }
   local emptyLine = ""
   while #lines[1] < chars do
      local events = pat:querySpan(cycle, cycle + 1)
      local filterfunc
      filterfunc = function(event)
         return event:hasOnset()
      end
      local events_with_onset = filter(filterfunc, events)
      local mapfunc
      mapfunc = function(event)
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
         local matches = filter(filterfunc, events)
         local missingLines = #matches - #lines
         if missingLines > 0 then
            for i = 1, missingLines do
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
return { drawline = drawline }
