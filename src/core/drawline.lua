local Time = require("modal.types").Time
local fun = require "modal.fun"
local ut = require "modal.utils"

local reduce = fun.reduce
local iter = fun.iter
local filter = ut.filter
local tconcat = table.concat

local gcd_reduce = function(tab)
   return reduce(function(acc, value)
      return acc:gcd(value)
   end, tab[1], tab)
end

local function map(func, items)
   local acc = {}
   for i, v in iter(items) do
      acc[i] = func(v, i)
   end
   return acc
end

local function drawLine(pat, chars)
   chars = chars or 60
   local cycle = 0
   local pos = Time(0)
   local lines = { "" }
   local emptyLine = ""
   while #lines[1] < chars do
      local events = pat(cycle, cycle + 1)
      local events_with_onset = filter(function(event)
         return event:hasOnset()
      end, events)
      local durations = map(function(ev)
         return ev:duration()
      end, events_with_onset)
      local charFraction = gcd_reduce(durations)
      local totalSlots = charFraction:reverse()
      lines = map(function(line)
         return line .. "|"
      end, lines)
      emptyLine = emptyLine .. "|"
      for _ = 1, totalSlots:asFloat() do
         local _begin, _end = pos, pos + charFraction
         local matches = filter(function(event)
            return event.whole._begin <= _begin and event.whole._end >= _end
         end, events)
         local missingLines = #matches - #lines
         if missingLines > 0 then
            for _ = 1, missingLines do
               lines = lines .. missingLines
            end
         end
         lines = map(function(line, index)
            local event = matches[index]
            if event ~= nil then
               local isOnset = event.whole._begin == _begin
               local char = nil
               if isOnset then
                  char = ut.dump(event.value)
               else
                  char = "-"
               end
               return line .. char
            end
            return line .. "."
         end, lines)
         emptyLine = emptyLine .. "."
         pos = pos + charFraction
      end
      cycle = cycle + 1
   end
   return tconcat(lines)
end

return drawLine
