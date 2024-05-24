local stack, slow, pure, fastcat, _patternify, _patternify_p_p, _patternify_p_p_p
do
   local _obj_0 = require("modal.pattern")
   stack, slow, pure, fastcat, _patternify, _patternify_p_p, _patternify_p_p_p =
      _obj_0.stack,
      _obj_0.slow,
      _obj_0.pure,
      _obj_0.fastcat,
      _obj_0._patternify,
      _obj_0._patternify_p_p,
      _obj_0._patternify_p_p_p
end
local Fraction, tofrac, tofloat
do
   local _obj_0 = require("modal.fraction")
   Fraction, tofrac, tofloat = _obj_0.Fraction, _obj_0.tofrac, _obj_0.tofloat
end
local map, filter, string_lambda, reduce, id, flatten, totable, dump, concat, rotate, union, timeToRand, curry, type
do
   local _obj_0 = require("modal.utils")
   map, filter, string_lambda, reduce, id, flatten, totable, dump, concat, rotate, union, timeToRand, curry, type =
      _obj_0.map,
      _obj_0.filter,
      _obj_0.string_lambda,
      _obj_0.reduce,
      _obj_0.id,
      _obj_0.flatten,
      _obj_0.totable,
      _obj_0.dump,
      _obj_0.concat,
      _obj_0.rotate,
      _obj_0.union,
      _obj_0.timeToRand,
      _obj_0.curry,
      _obj_0.type
end
local Event, Span, State
do
   local _obj_0 = require("modal.types")
   Event, Span, State = _obj_0.Event, _obj_0.Span, _obj_0.State
end

local M = {}
local U = {}
local register = require("modal.pattern").register

local P = require("modal.params")

register("juxBy", function(by, f, pat)
   by = by / 2
   local elem_or
   elem_or = function(dict, key, default)
      if dict[key] ~= nil then
         return dict[key]
      end
      return default
   end
   local left = pat:fmap(function(valmap)
      return union({
         pan = elem_or(valmap, "pan", 0.5) - by,
      }, valmap)
   end)
   local right = pat:fmap(function(valmap)
      return union({
         pan = elem_or(valmap, "pan", 0.5) + by,
      }, valmap)
   end)
   return stack(left, f(right))
end)

register("jux", function(f, pat)
   return U.juxBy(0.5, f, pat)
end)

register("striate", function(n, pat)
   local ranges
   do
      local _accum_0 = {}
      local _len_0 = 1
      for i = 0, n - 1 do
         _accum_0[_len_0] = {
            begin = i / n,
            ["end"] = (i + 1) / n,
         }
         _len_0 = _len_0 + 1
      end
      ranges = _accum_0
   end
   local merge_sample
   merge_sample = function(range)
      local f
      f = function(v)
         return union(range, {
            sound = v.sound,
         })
      end
      return pat:fmap(f)
   end
   return fastcat((function()
      local _accum_0 = {}
      local _len_0 = 1
      for _index_0 = 1, #ranges do
         local r = ranges[_index_0]
         _accum_0[_len_0] = merge_sample(r)
         _len_0 = _len_0 + 1
      end
      return _accum_0
   end)())
end)

register("chop", function(n, pat)
   local ranges
   do
      local _accum_0 = {}
      local _len_0 = 1
      for i = 0, n - 1 do
         _accum_0[_len_0] = {
            begin = i / n,
            ["end"] = (i + 1) / n,
         }
         _len_0 = _len_0 + 1
      end
      ranges = _accum_0
   end
   local func
   func = function(o)
      local f
      f = function(slice)
         return union(slice, o)
      end
      return fastcat(map(f, ranges))
   end
   return pat:squeezeBind(func)
end)

register("slice", function(npat, ipat, opat)
   return npat:innerBind(function(n)
      return ipat:outerBind(function(i)
         return opat:outerBind(function(o)
            local begin
            if type(n) == table then
               begin = n[i]
            else
               begin = i / n
            end
            local _end
            if type(n) == table then
               _end = n[i + 1]
            else
               _end = (i + 1) / n
            end
            return pure(union(o, {
               begin = begin,
               ["end"] = _end,
               _slices = n,
            }))
         end)
      end)
   end)
end)

register("splice", function(npat, ipat, opat)
   local sliced = M.slice(npat, ipat, opat)
   return sliced:withEvent(function(event)
      return event:withValue(function(value)
         local new_attri = {
            speed = tofloat(tofrac(1) / tofrac(value._slices) / event.whole:duration()) * (value.speed or 1),
            unit = "c",
         }
         return union(new_attri, value)
      end)
   end)
end)

register("_loopAt", function(factor, pat)
   pat = pat .. P.speed(1 / factor) .. P.unit("c")
   return slow(factor, pat)
end)

register("fit", function(pat)
   return pat:withEvent(function(event)
      return event:withValue(function(value)
         return union(value, {
            speed = tofrac(1) / event.whole:duration(),
            unit = "c",
         })
      end)
   end)
end)

register("legato", function(factor, pat)
   factor = tofrac(factor)
   return pat:withEventSpan(function(span)
      return Span(span._begin, (span._begin + span:duration() * factor))
   end)
end)

return M
