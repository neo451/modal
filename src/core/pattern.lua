local map, filter, string_lambda, reduce, id, flatten, totable, dump, union, timeToRand, curry, T, nparams, concat2
do
   local utils = require("modal.utils")
   map, filter, string_lambda, reduce, id, flatten, totable, dump, union, timeToRand, curry, T, nparams, concat2 =
      utils.map,
      utils.filter,
      utils.string_lambda,
      utils.reduce,
      utils.id,
      utils.flatten,
      utils.totable,
      utils.dump,
      utils.union,
      utils.timeToRand,
      utils.curry,
      utils.type,
      utils.nparams,
      utils.concat2
end
local bjork = require("modal.euclid").bjork
local getScale = require("modal.scales").getScale
local Fraction, tofrac, tofloat
do
   local frac = require("modal.fraction")
   Fraction, tofrac, tofloat = frac.Fraction, frac.tofrac, frac.tofloat
end
local Event, Span, State
do
   local types = require("modal.types")
   Event, Span, State = types.Event, types.Span, types.State
end

-- local parse = require("modal.mini").parse
local reify, pure, silence
local bindWhole, bind, innerBind, outerBind, innerJoin, outerJoin
local fmap, firstCycle, querySpan
local combineRight, combineLeft
local withEventTime
local appLeft, appRight
local squeezeJoin, squeezeBind, filterEvents, filterValues, removeNils, splitQueries, withQueryTime, withQuerySpan, withTime, withEvents, withEvent, withEventSpan, onsetsOnly, discreteOnly, withValue

local fun = require("modal.fun")
local sin = math.sin
local min = math.min
local max = math.max
local pi = math.pi
local floor = math.floor
local tinsert = table.insert

--- Unpatternified versions
local M = {}
local U = {}

M.mini = require("modal.mini")(M)

function fmap(pat, func)
   return withValue(pat, func)
end
M.fmap = fmap

function querySpan(pat, b, e)
   local span = Span(b, e)
   local state = State(span)
   return pat:query(state)
end

function firstCycle(pat)
   return pat(0, 1)
end

combineRight = function(pat, other)
   return appLeft(
      fmap(pat, function(x)
         return function(y)
            return union(y, x)
         end
      end),
      other
   )
end

combineLeft = function(pat, other)
   return appLeft(
      fmap(pat, function(x)
         return function(y)
            return union(x, y)
         end
      end),
      other
   )
end

appLeft = function(pat, pat_val)
   local query = function(_, state)
      local events = {}
      local event_funcs = pat:query(state)
      for _index_0 = 1, #event_funcs do
         local event_func = event_funcs[_index_0]
         local whole = event_func:wholeOrPart()
         local event_vals = pat_val(whole._begin, whole._end)
         for _index_1 = 1, #event_vals do
            local event_val = event_vals[_index_1]
            local new_whole = event_func.whole
            local new_part = event_func.part:sect(event_val.part)
            local new_context = event_val:combineContext(event_func)
            if new_part ~= nil then
               local new_value = event_func.value(event_val.value)
               tinsert(events, Event(new_whole, new_part, new_value, new_context))
            end
         end
      end
      return events
   end
   return Pattern(query)
end

appRight = function(pat, pat_val)
   local query = function(_, state)
      local events = {}
      local event_vals = pat_val:query(state)
      for _index_0 = 1, #event_vals do
         local event_val = event_vals[_index_0]
         local whole = event_val:wholeOrPart()
         local event_funcs = pat(whole._begin, whole._end)
         event_val:wholeOrPart()
         for _index_1 = 1, #event_funcs do
            local event_func = event_funcs[_index_1]
            local new_whole = event_val.whole
            local new_part = event_func.part:sect(event_val.part)
            local new_context = event_val:combineContext(event_func)
            if new_part ~= nil then
               local new_value = event_func.value(event_val.value)
               tinsert(events, Event(new_whole, new_part, new_value, new_context))
            end
         end
         return events
      end
   end
   return Pattern(query)
end

bindWhole = function(pat, choose_whole, func)
   local query = function(_, state)
      local withWhole = function(a, b)
         local new_whole = choose_whole(a.whole, b.whole)
         return Event(new_whole, b.part, b.value)
      end
      local match = function(a)
         local events = func(a.value)(a.part._begin, a.part._end)
         local f
         f = function(b)
            return withWhole(a, b)
         end
         return map(f, events)
      end
      local events = pat:query(state)
      return flatten((map(function(a)
         return match(a)
      end, events)))
   end
   return Pattern(query)
end

bind = function(pat, func)
   local whole_func
   whole_func = function(a, b)
      if a == nil or b == nil then
         return nil
      end
      return a:sect(b)
   end
   return bindWhole(pat, whole_func, func)
end

outerBind = function(pat, func)
   return bindWhole(pat, function(a)
      return a
   end, func)
end

innerBind = function(pat, func)
   return bindWhole(pat, function(_, b)
      return b
   end, func)
end

outerJoin = function(pat)
   return outerBind(pat, id)
end
M.outerJoin = outerJoin

innerJoin = function(pat)
   return innerBind(pat, id)
end

squeezeJoin = function(pat)
   local query
   query = function(_, state)
      local events = discreteOnly(pat):query(state)
      local flatEvent
      flatEvent = function(outerEvent)
         local innerPat = M.focusSpan(outerEvent:wholeOrPart(), outerEvent.value)
         local innerEvents = innerPat:query(State(outerEvent.part))
         local munge
         munge = function(outer, inner)
            local whole = nil
            if inner.whole and outer.whole then
               whole = inner.whole:sect(outer.whole)
               if not whole then
                  return nil
               end
            end
            local part = inner.part:sect(outer.part)
            if not part then
               return nil
            end
            return Event(whole, part, inner.value)
         end
         local f
         f = function(innerEvent)
            return munge(outerEvent, innerEvent)
         end
         return map(f, innerEvents)
      end
      local result = flatten(map(flatEvent, events))
      return filter(function(x)
         return x
      end, result)
   end
   return Pattern(query)
end

squeezeBind = function(pat, func)
   return squeezeJoin(fmap(pat, func))
end

filterEvents = function(pat, func)
   local query
   query = function(_, state)
      local events = pat:query(state)
      return filter(func, events)
   end
   return Pattern(query)
end

filterValues = function(pat, condf)
   local query
   query = function(_, state)
      local events = pat:query(state)
      local f
      f = function(event)
         return condf(event.value)
      end
      return filter(f, events)
   end
   return Pattern(query)
end

removeNils = function(pat)
   return filterValues(pat, function(v)
      return v ~= nil
   end)
end
splitQueries = function(pat)
   local query
   query = function(_, state)
      local cycles = state.span:spanCycles()
      local f
      f = function(span)
         local sub_state = state:setSpan(span)
         return pat:query(sub_state)
      end
      return flatten(map(f, cycles))
   end
   return Pattern(query)
end

withValue = function(pat, func)
   local query = function(_, state)
      local events = pat:query(state)
      local f
      f = function(event)
         return event:withValue(func)
      end
      return map(f, events)
   end
   return Pattern(query)
end

withQuerySpan = function(pat, func)
   local query
   query = function(_, state)
      local new_state = state:withSpan(func)
      return pat:query(new_state)
   end
   return Pattern(query)
end

withQueryTime = function(pat, func)
   return withQuerySpan(pat, function(span)
      return span:withTime(func)
   end)
end

withTime = function(pat, qf, ef)
   local query = withQueryTime(pat, qf)
   local pattern = withEventTime(query, ef)
   return pattern
end

withEvents = function(pat, func)
   return Pattern(function(_, state)
      return func(pat:query(state))
   end)
end

withEvent = function(pat, func)
   return withEvents(pat, function(events)
      return map(func, events)
   end)
end

withEventSpan = function(pat, func)
   local query
   query = function(_, state)
      local events = pat:query(state)
      local f
      f = function(event)
         return event:withSpan(func)
      end
      return map(f, events)
   end
   return Pattern(query)
end

withEventTime = function(pat, func)
   local query
   query = function(_, state)
      local events = pat:query(state)
      local time_func
      time_func = function(span)
         return span:withTime(func)
      end
      local event_func
      event_func = function(event)
         return event:withSpan(time_func)
      end
      return map(event_func, events)
   end
   return Pattern(query)
end

onsetsOnly = function(pat)
   return filterEvents(pat, function(event)
      return event:hasOnset()
   end)
end
M.onsetsOnly = onsetsOnly

discreteOnly = function(pat)
   return filterEvents(pat, function(event)
      return event.whole
   end)
end

local base = {}

function base:type()
   return "pattern"
end

function base:__call(b, e)
   return querySpan(self, b, e)
end

function base:__tostring()
   return dump(firstCycle(self))
end

function base:__concat(other)
   return combineLeft(self, other)
end

function base:__eq(other)
   return self:__tostring() == other:__tostring()
end

function base:__mul(other)
   return appLeft(
      fmap(self, function(x)
         return function(y)
            return y * x
         end
      end),
      reify(other)
   )
end

function base:__div(other)
   return appLeft(
      fmap(self, function(x)
         return function(y)
            return y / x
         end
      end),
      reify(other)
   )
end

function base:__add(other)
   return appLeft(
      fmap(self, function(x)
         return function(y)
            return y + x
         end
      end),
      reify(other)
   )
end

function base:__sub(other)
   return appLeft(
      fmap(self, function(x)
         return function(y)
            return y - x
         end
      end),
      reify(other)
   )
end

function base:__mod(other)
   return appLeft(
      fmap(self, function(x)
         return function(y)
            return y % x
         end
      end),
      reify(other)
   )
end

function base:__pow(other)
   return appLeft(
      fmap(self, function(x)
         return function(y)
            return y ^ x
         end
      end),
      reify(other)
   )
end

base.__index = base

Pattern = {
   __base = base,
   __name = "Pattern",
}
M.Pattern = Pattern

function Pattern:__init(query)
   if query == nil then
      query = function()
         return {}
      end
   end
   self.query = query
end

setmetatable(Pattern, {
   __index = base,
   __call = function(cls, ...)
      local _self_0 = setmetatable({}, base)
      cls.__init(_self_0, ...)
      return _self_0
   end,
})

base.__class = Pattern

---@alias pattern function # specify

function silence()
   return Pattern()
end
M.silence = silence

function pure(value)
   local query = function(_, state)
      local cycles = state.span:spanCycles()
      local f = function(span)
         local whole = span:wholeCycle(span._begin)
         return Event(whole, span, value)
      end
      return map(f, cycles)
   end
   return Pattern(query)
end

M.pure = pure

local env = concat2(M, _G)
reify = function(thing)
   local t = T(thing)
   if "string" == t then
      local res = M.eval("(" .. thing .. ")", env)
      return res
   elseif "pattern" == t then
      return thing
   else
      return pure(thing)
   end
end

M.reify = reify

local patternify = { id }

--- TODO: combine into one!!
patternify[2] = function(func)
   local patterned = function(apat, pat)
      if pat == nil then
         return curry(func, 2)(apat)
      end
      apat, pat = reify(apat), reify(pat)
      local mapFn = function(a)
         return func(a, pat)
      end
      return innerJoin(fmap(apat, mapFn))
   end
   return patterned
end

patternify[3] = function(func)
   local patterned = function(apat, bpat, pat)
      if pat == nil then
         return curry(func, 3)(apat)(bpat)
      end
      apat, bpat, pat = reify(apat), reify(bpat), reify(pat)
      local mapFn = function(a, b)
         return func(a, b, pat)
      end
      mapFn = curry(mapFn, 2)
      local patOfPats = appLeft(fmap(apat, mapFn), bpat)
      return innerJoin(patOfPats)
   end
   return patterned
end

patternify[4] = function(func)
   local patterned = function(apat, bpat, cpat, pat)
      if pat == nil then
         return curry(func, 4)(apat)(bpat)(cpat)
      end
      apat, bpat, cpat, pat = reify(apat), reify(bpat), reify(cpat), reify(pat)
      local mapFn = function(a, b, c)
         return func(a, b, c, pat)
      end
      mapFn = curry(mapFn, 3)
      local patOfPats = appLeft(appLeft(fmap(apat, mapFn), bpat), cpat)
      return innerJoin(patOfPats)
   end
   return patterned
end

local function register(name, f)
   local narg = nparams(f)
   U[name] = f
   M[name] = patternify[narg](f)
end
M.register = register

function M.stack(...)
   local pats = map(reify, totable(...))
   local query = function(_, state)
      local span = state.span
      local f
      f = function(pat)
         return pat(span._begin, span._end)
      end
      local events = map(f, pats)
      return flatten(events)
   end
   return Pattern(query)
end

function M.superimpose(f, pat)
   return M.stack(pat, M.sl(f)(pat))
end

function M.layer(tf, pat)
   local acc = {}
   for i = 1, #tf do
      local f = tf[i]
      acc[i] = M.sl(f)(pat)
   end
   return M.stack(acc)
end

-- slowcatPrime = function(...)
-- 	local pats = map(reify, totable(...))
-- 	local query = function(_, state)
-- 		local len = #pats
-- 		local index = state.span._begin:sam():asFloat() % len + 1
-- 		local pat = pats[index]
-- 		return pat:query(state)
-- 	end
-- 	return Pattern(query):splitQueries()
-- end

function M.slowcat(...)
   -- local pats = map(reify, totable(...))
   local pats = totable(...)
   local query
   query = function(_, state)
      local span = state.span
      local len = #pats
      local index = state.span._begin:sam():asFloat() % len + 1
      local pat = pats[index]
      if not pat then
         return {}
      end
      local offset = span._begin:floor() - (span._begin / len):floor()
      return withEventTime(pat, function(t)
         return t + offset
      end):query(state:setSpan(span:withTime(function(t)
         return t - offset
      end)))
   end
   return splitQueries(Pattern(query))
end

---Like slowcat, but the items are crammed into one cycle.
---@param ... any
---@return pattern
---@usage
---fastcat("e5", "b4", "d5", "c5") // "e5 b4 d5 c5"
function M.fastcat(...)
   local pats = totable(...)
   return U.fast(#pats, M.slowcat(...))
end

function M.timecat(tuples)
   local pats = {}
   local times = map(function(x)
      return x[1]
   end, tuples)
   local total = reduce(fun.op.add, Fraction(0), times)
   local accum = Fraction(0)
   for i = 1, #tuples do
      local tup = tuples[i]
      local time, pat = tup[1], tup[2]
      local b = accum / total
      local e = (accum + time) / total
      local new_pat = U.compress(b, e, pat)
      tinsert(pats, new_pat)
      accum = accum + time
   end
   return M.stack(pats)
end

register("fast", function(factor, pat)
   return withTime(pat, function(t)
      return t * factor
   end, function(t)
      return t / factor
   end)
end)

register("slow", function(factor, pat)
   return U.fast(1 / factor, pat)
end)

register("early", function(offset, pat)
   return withTime(pat, function(t)
      return t + offset
   end, function(t)
      return t - offset
   end)
end)

register("late", function(offset, pat)
   return U.early(-offset, pat)
end)

register("inside", function(factor, f, pat)
   return U.fast(factor, f(U.slow(factor, pat)))
end)

register("outside", function(factor, f, pat)
   return U.inside(1 / factor, f, pat)
end)

register("ply", function(factor, pat)
   pat = pure(U.fast(factor, pat))
   return squeezeJoin(pat)
end)

register("fastgap", function(factor, pat)
   factor = tofrac(factor)
   if factor <= Fraction(0) then
      return M.silence()
   end
   factor = factor:max(1)
   local mungeQuery = function(t)
      return t:sam() + ((t - t:sam()) * factor):min(1)
   end
   local eventSpanFunc = function(span)
      local b = span._begin:sam() + (span._begin - span._begin:sam()) / factor
      local e = span._begin:sam() + (span._end - span._begin:sam()) / factor
      return Span(b, e)
   end
   local query = function(_, state)
      local span = state.span
      local new_span = Span(mungeQuery(span._begin), mungeQuery(span._end))
      if new_span._begin == new_span._begin:nextSam() then
         return {}
      end
      local new_state = State(new_span)
      local events = pat:query(new_state)
      local f
      f = function(event)
         return event:withSpan(eventSpanFunc)
      end
      return map(f, events)
   end
   return splitQueries(Pattern(query))
end)

register("compress", function(b, e, pat)
   b, e = tofrac(b), tofrac(e)
   if b > e or e > Fraction(1) or b > Fraction(1) or b < Fraction(0) or e < Fraction(0) then
      return M.silence()
   end
   local fasted = U.fastgap((Fraction(1) / (e - b)), pat)
   return U.late(b, fasted)
end)

register("focus", function(b, e, pat)
   b, e = tofrac(b), tofrac(e)
   local fasted = U.fast((Fraction(1) / (e - b)), pat)
   return U.late(Span:cyclePos(b), fasted)
end)

-- TODO: replace
M.focusSpan = function(span, pat)
   return U.focus(span._begin, span._end, reify(pat))
end

register("zoom", function(s, e, pat)
   s, e = tofrac(s), tofrac(e)
   local dur = e - s
   local qf = function(span)
      return span:withCycle(function(t)
         return t * dur + s
      end)
   end
   local ef = function(span)
      return span:withCycle(function(t)
         return (t - s) / dur
      end)
   end
   -- HACK:
   return splitQueries(withEventSpan(withQuerySpan(pat, qf), ef))
end)

register("run", function(n)
   return M.fastcat(fun.totable(fun.range(0, n - 1)))
end)

--- HACK: internal??
register("iota", function(b, e)
   return M.fastcat(fun.totable(fun.range(b, e)))
end)

register("scan", function(n)
   return M.slowcat(map(U.run, fun.range(1, n)))
end)

local waveform = function(func)
   local query
   query = function(_, state)
      return {
         Event(nil, state.span, func(state.span:midpoint())),
      }
   end
   return Pattern(query)
end

M.steady = function(value)
   return Pattern(function(state)
      return {
         Event(nil, state.span, value),
      }
   end)
end

local toBipolar = function(pat)
   return fmap(pat, function(x)
      return x * 2 - 1
   end)
end

local fromBipolar = function(pat)
   return fmap(pat, function(x)
      return (x + 1) / 2
   end)
end

M.sine2 = waveform(function(t)
   return sin(t:asFloat() * pi * 2)
end)
M.sine = fromBipolar(M.sine2)
M.cosine2 = U.late(1 / 4, M.sine2)
M.cosine = fromBipolar(M.cosine2)
M.square = waveform(function(t)
   return floor((t * 2) % 2)
end)
M.square2 = toBipolar(M.square)
M.isaw = waveform(function(t)
   return -(t % 1) + 1
end)
M.isaw2 = toBipolar(M.isaw)
M.saw = waveform(function(t)
   return t % 1
end)
M.saw2 = toBipolar(M.saw)
M.tri = M.fastcat(M.isaw, M.saw)
M.tri2 = M.fastcat(M.isaw2, M.saw2)
M.time = waveform(id)
M.rand = waveform(timeToRand)
M._irand = function(i)
   return fmap(M.rand, function(x)
      return floor(x * i)
   end)
end
M.irand = function(ipat)
   return innerJoin(fmap(reify(ipat), M._irand))
end
local _chooseWith = function(pat, ...)
   local vals = map(reify, totable(...))
   if #vals == 0 then
      return M.silence()
   end
   return fmap(M.range(1, #vals + 1, pat), function(i)
      local key = min(max(floor(i), 0), #vals)
      return vals[key]
   end)
end
local chooseWith = function(pat, ...)
   return outerJoin(_chooseWith(pat, ...))
end
local chooseInWith = function(pat, ...)
   return innerJoin(_chooseWith(pat, ...))
end
local choose = function(...)
   return chooseInWith(M.rand, ...)
end
local chooseCycles = function(...)
   return M.segment(1, choose(...))
end

M.randcat = function(...)
   return chooseCycles(...)
end

register("degradeByWith", function(prand, by, pat)
   if T(by) == "fraction" then
      by = by:asFloat()
   end
   local f = function(v)
      return v > by
   end
   return appLeft(
      fmap(pat, function(val)
         return function(_)
            return val
         end
      end),
      filterValues(prand, f)
   )
end)

register("degradeBy", function(by, pat)
   return U.degradeByWith(M.rand, by, pat)
end)

register("undegradeBy", function(by, pat)
   return U.degradeByWith(
      fmap(M.rand, function(r)
         return 1 - r
      end),
      by,
      pat
   )
end)

register("degrade", function(pat)
   return U.degradeBy(0.5, pat)
end)

register("undegrade", function(pat)
   return U.undegradeBy(0.5, pat)
end)

register("sometimesBy", function(by, func, pat)
   return innerJoin(fmap(reify(by), function()
      return M.stack(U.degradeBy(by, pat), func(U.undegradeBy(1 - by, pat)))
   end))
end)

register("sometimes", function(func, pat)
   return U.sometimesBy(0.5, func, pat)
end)

function M.struct(boolpat, pat)
   local f = function(b)
      return function(val)
         return b and val or nil
      end
   end
   local bools = M.fastcat(boolpat)
   return removeNils(appLeft(fmap(bools, f), pat))
end

register("euclid", function(n, k, offset, pat)
   return M.struct(bjork(n, k, offset), reify(pat))
end)

register("rev", function(pat)
   local query = function(_, state)
      local span = state.span
      local cycle = span._begin:sam()
      local nextCycle = span._begin:nextSam()
      local reflect
      reflect = function(to_reflect)
         local reflected = to_reflect:withTime(function(t)
            return cycle + (nextCycle - t)
         end)
         local tmp = reflected._begin
         reflected._begin = reflected._end
         reflected._end = tmp
         return reflected
      end
      local events = pat:query(state:setSpan(reflect(span)))
      return map(function(event)
         return event:withSpan(reflect)
      end, events)
   end
   return Pattern(query)
end)

register("palindrome", function(pat)
   return M.slowcat(pat, U.rev(pat))
end)

register("iter", function(n, pat)
   local acc = {}
   for i in fun.range(n) do
      acc[i] = U.early(Fraction(i - 1, n), pat)
   end
   return M.slowcat(acc)
end)

register("reviter", function(n, pat)
   local acc = {}
   for i in fun.range(n) do
      acc[i] = U.late(Fraction(i - 1, n), pat)
   end
   return M.slowcat(acc)
end)

register("segment", function(n, pat)
   return appLeft(M.fast(n, pure(id)), pat)
end)

register("range", function(mi, ma, pat)
   return fmap(pat, function(x)
      return x * (ma - mi) + mi
   end)
end)

register("off", function(time_pat, f, pat)
   return M.stack(pat, f(M.late(time_pat, pat)))
end)

register("echoWith", function(times, time, func, pat)
   local f
   f = function(index)
      return func(U.late(time * index, pat))
   end
   local ts
   do
      local _accum_0 = {}
      local _len_0 = 1
      for i = 0, times - 1 do
         _accum_0[_len_0] = i
         _len_0 = _len_0 + 1
      end
      ts = _accum_0
   end
   return M.stack(map(f, ts))
end)

register("when", function(bool, f, pat)
   return bool and f(pat) or pat
end)

register("firstOf", function(n, f, pat)
   local acc = {}
   for i = 1, n do
      acc[i] = i == 1 and true or false
   end
   return M.when(M.fastcat(acc), f, pat)
end)

M.every = M.firstOf

register("lastOf", function(n, f, pat)
   local acc = {}
   for i = 1, n do
      acc[i] = i == n and true or false
   end
   return M.when(M.fastcat(acc), f, pat)
end)

register("scale", function(name, pat)
   local toScale = function(v)
      return getScale(name, v)
   end
   return fmap(pat, toScale)
end)

M.app = function(x, pat)
   return pat .. x
end

M.sl = string_lambda

M.print = function(x)
   if type(x) == "table" then
      for k, v in pairs(x) do
         print(k, v)
      end
   else
      return print(x)
   end
end

M.id = id
local maxi = require("modal.maxi")
M.eval = maxi.eval
M.to_lua = maxi.to_lua
M.T = T

return M
