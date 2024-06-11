local ut = require "modal.utils"
local reduce, map, filter, id, flatten, union, timeToRand, curry, T, nparams, flip, method_wrap, auto_curry =
   ut.reduce,
   ut.map,
   ut.filter,
   ut.id,
   ut.flatten,
   ut.union,
   ut.timeToRand,
   ut.curry,
   ut.type,
   ut.nparams,
   ut.flip,
   ut.method_wrap,
   ut.auto_curry
local bjork = require("modal.euclid").bjork
local getScale = require("modal.scales").getScale
local frac = require "modal.fraction"
local Fraction, tofrac = frac.Fraction, frac.tofrac
local types = require "modal.types"
local Event, Span, State = types.Event, types.Span, types.State
local TDef = require "modal.type_def"
local maxi = require "modal.maxi"
local fun = require "modal.fun"

local Pattern
local reify, pure, silence, overlay
local bindWhole, bind, innerBind, outerBind, innerJoin, outerJoin, join
local fmap, firstCycle, querySpan
local withEventTime
local appLeft, appRight, appBoth
local squeezeJoin, squeezeBind, filterEvents, filterValues, removeNils, splitQueries, withQueryTime, withQuerySpan, withTime, withEvents, withEvent, withEventSpan, onsetsOnly, discreteOnly, withValue

local iter = fun.iter
local sin = math.sin
local min = math.min
local max = math.max
local pi = math.pi
local floor = math.floor
---@alias State table
---@alias Event<T> {value: T}
---@alias Pattern<T> fun(st: State): Event<T>[]

local M = {}
local U = {} -- Unpatternified versions
local TYPES = {}
M.mini = maxi(M, false)
M.sl = ut.string_lambda(M)

local _op = {}
function _op.In(f)
   return function(a, b)
      a, b = fmap(reify(a), curry(f, 2)), reify(b)
      return appLeft(a, b):removeNils()
   end
end

function _op.Out(f)
   return function(a, b)
      a, b = fmap(reify(a), curry(f, 2)), reify(b)
      return appRight(a, b):removeNils()
   end
end

function _op.Mix(f)
   return function(a, b)
      a, b = fmap(reify(a), curry(f, 2)), reify(b)
      return appBoth(a, b):removeNils()
   end
end

function _op.Squeeze(f)
   return function(a, b)
      return squeezeJoin(fmap(reify(a), function(c)
         return fmap(reify(b), function(d)
            return f(c, d)
         end)
      end)):removeNils()
   end
end

function _op.SqueezeOut(f)
   return function(a, b)
      return squeezeJoin(fmap(reify(b), function(c)
         return fmap(reify(a), function(d)
            return f(d, c)
         end)
      end)):removeNils()
   end
end

-- stylua: ignore start
local ops = {
   add = function(a, b) return a + b end,
   sub = function(a, b) return a - b end,
   mul = function(a, b) return a * b end,
   div = function(a, b) return a / b end,
   mod = function(a, b) return a % b end,
   pow = function(a, b) return a ^ b end,
   concat = function (a, b) return a .. b end,
   keepif = function (a, b) return b and a or nil end,
   uni = function (a, b) return union(a, b) end,
   funi = function (a, b) return flip(union)(a, b) end,
}
-- stylua: ignore end

-- local hows = { "In", "Out", "Mix", "Squeeze", "Squeezeout", "Trig", "Trigzero" }
local hows = { "In", "Out", "Mix", "Squeeze", "SqueezeOut" }
local op_set = {
   add = "+",
   sub = "-",
   mul = "*",
   div = "/",
   mod = "%",
   pow = "^",
   keepif = "?",
   concat = "..", -- ?
   uni = "<",
   funi = ">",
}

local how_format = {
   In = "|%s",
   Out = "%s|",
   Mix = "|%s|",
   Squeeze = "||%s",
   SqueezeOut = "%s||",
}

local op = {}
for k, f in pairs(ops) do
   op[k] = {}
   for _, v in ipairs(hows) do
      op[k][v] = _op[v](f)
      if op_set[k] and how_format[v] then
         local symb = string.format(how_format[v], op_set[k])
         op[symb] = _op[v](f)
      end
   end
end

M.op = op
local base = {}

function base:type()
   return "pattern"
end

function base:__call(b, e)
   return querySpan(b, e, self)
end

function base:__tostring()
   return ut.dump2(firstCycle(self))
end

function base:__concat(other)
   return M.op["|>"](self, other)
end

function base:__add(other)
   return M.op["|+"](self, other)
end

function base:__sub(other)
   return M.op["|-"](self, other)
end

function base:__mul(other)
   return M.op["|*"](self, other)
end

function base:__div(other)
   return M.op["|/"](self, other)
end

function base:__mod(other)
   return M.op["|%"](self, other)
end

function base:__pow(other)
   return M.op["|^"](self, other)
end

function base:__eq(other)
   return self:__tostring() == other:__tostring()
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

function fmap(pat, func)
   return withValue(pat, func)
end
M.fmap = fmap

function querySpan(b, e, pat)
   local span = Span(b, e)
   local state = State(span)
   return pat:query(state)
end
base.fmap = fmap
M.querySpan = querySpan

function firstCycle(pat)
   return pat(0, 1)
end

local appWhole = function(pat, whole_func, pat_val)
   local query = function(_, state)
      local event_funcs = pat:query(state)
      local event_vals = pat_val:query(state)
      local apply = function(event_func, event_val)
         local new_part = event_func.part:sect(event_val.part)
         if not new_part then
            return
         end
         return Event(
            whole_func(event_func.whole, event_val.whole),
            new_part,
            event_func.value(event_val.value),
            event_val:combineContext(event_func)
         )
      end
      local events = {}
      for _, ef in pairs(event_funcs) do
         for _, ev in ipairs(event_vals) do
            events[#events + 1] = apply(ef, ev)
         end
      end
      return events
   end
   return Pattern(query)
end

-- Tidal's <*>
appBoth = function(pat, pat_val)
   local whole_func = function(span_a, span_b)
      if not span_a or not span_b then
         return
      end
      return span_a:sect(span_b)
   end
   return appWhole(pat, whole_func, pat_val)
end
base.appBoth = appBoth

-- Tidal's <*
appLeft = function(pat, pat_val)
   local query = function(_, state)
      local events = {}
      local event_funcs = pat:query(state)
      for _, event_func in ipairs(event_funcs) do
         local whole = event_func:wholeOrPart()
         local event_vals = pat_val:query(state:setSpan(whole))
         for _, event_val in ipairs(event_vals) do
            local new_whole = event_func.whole
            local new_part = event_func.part:sect(event_val.part)
            local new_context = event_val:combineContext(event_func)
            if new_part then
               local new_value = event_func.value(event_val.value)
               events[#events + 1] = Event(new_whole, new_part, new_value, new_context)
            end
         end
      end
      return events
   end
   return Pattern(query)
end
base.appLeft = appLeft

-- Tidal's *>
appRight = function(pat, pat_val)
   local query = function(_, state)
      local events = {}
      local event_vals = pat_val:query(state)
      for _, event_val in ipairs(event_vals) do
         local whole = event_val:wholeOrPart()
         local event_funcs = pat:query(state:setSpan(whole))
         for _, event_func in ipairs(event_funcs) do
            local new_whole = event_val.whole
            local new_part = event_func.part:sect(event_val.part)
            if new_part then
               local new_value = event_func.value(event_val.value)
               local new_context = event_val:combineContext(event_func)
               events[#events + 1] = Event(new_whole, new_part, new_value, new_context)
            end
         end
      end
      return events
   end
   return Pattern(query)
end
base.appRight = appRight

bindWhole = function(pat, choose_whole, func)
   local query = function(_, state)
      local withWhole = function(a, b)
         local new_whole = choose_whole(a.whole, b.whole)
         return Event(new_whole, b.part, b.value)
      end
      local match = function(a)
         local events = func(a.value):query(state:setSpan(a.part))
         local f = function(b)
            return withWhole(a, b)
         end
         return map(f, events)
      end
      local events = pat:query(state)
      return flatten((map(match, events)))
   end
   return Pattern(query)
end
base.bindWhole = bindWhole

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
base.bind = bind

join = function(pat)
   return bind(pat, id)
end
base.join = join

outerBind = function(pat, func)
   return bindWhole(pat, function(a)
      return a
   end, func)
end
base.outerBind = outerBind

innerBind = function(pat, func)
   return bindWhole(pat, function(_, b)
      return b
   end, func)
end
base.innerBind = innerBind

outerJoin = function(pat)
   return outerBind(pat, id)
end
base.outerJoin = outerJoin

innerJoin = function(pat)
   return innerBind(pat, id)
end
base.innerJoin = innerJoin

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
base.squeezeJoin = squeezeJoin

squeezeBind = function(pat, func)
   return squeezeJoin(fmap(pat, func))
end
base.squeezeBind = squeezeBind

filterEvents = function(pat, func)
   local query = function(_, state)
      local events = pat:query(state)
      return filter(func, events)
   end
   return Pattern(query)
end
base.filterEvents = filterEvents

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
base.filterValues = filterValues

removeNils = function(pat)
   return filterValues(pat, function(v)
      return v ~= nil
   end)
end
base.removeNils = removeNils

splitQueries = function(pat)
   local query = function(_, state)
      local cycles = state.span:spanCycles()
      local f = function(subspan)
         return pat:query(state:setSpan(subspan))
      end
      return flatten(map(f, cycles))
   end
   return Pattern(query)
end
base.splitQueries = splitQueries

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
base.withValue = withValue

withQuerySpan = function(pat, func)
   local query
   query = function(_, state)
      local new_state = state:withSpan(func)
      return pat:query(new_state)
   end
   return Pattern(query)
end
base.withQuerySpan = withQuerySpan

withQueryTime = function(pat, func)
   return withQuerySpan(pat, function(span)
      return span:withTime(func)
   end)
end
base.withQueryTime = withQueryTime

withTime = function(pat, qf, ef)
   local query = withQueryTime(pat, qf)
   local pattern = withEventTime(query, ef)
   return pattern
end
base.withTime = withTime

withEvents = function(pat, func)
   return Pattern(function(_, state)
      return func(pat:query(state))
   end)
end
base.withEvents = withEvents

withEvent = function(pat, func)
   return withEvents(pat, function(events)
      return map(func, events)
   end)
end
base.withEvent = withEvent

withEventSpan = function(pat, func)
   local query = function(_, state)
      local events = pat:query(state)
      return map(function(ev)
         return ev:withSpan(func)
      end, events)
   end
   return Pattern(query)
end
base.withEventSpan = withEventSpan

withEventTime = function(pat, func)
   local query = function(_, state)
      local events = pat:query(state)
      local time_func = function(span)
         return span:withTime(func)
      end
      local event_func = function(event)
         return event:withSpan(time_func)
      end
      return map(event_func, events)
   end
   return Pattern(query)
end
base.withEventTime = withEventTime

onsetsOnly = function(pat)
   return filterEvents(pat, function(event)
      return event:hasOnset()
   end)
end
M.onsetsOnly = onsetsOnly
base.onsetsOnly = onsetsOnly

discreteOnly = function(pat)
   return filterEvents(pat, function(event)
      return event.whole
   end)
end
base.discreteOnly = discreteOnly

---@alias pattern function # specify
function silence()
   return Pattern()
end
M.silence = silence

function pure(value)
   if value == "~" then
      return silence()
   end
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

reify = function(thing)
   local t = T(thing)
   if "string" == t then
      local res = M.mini("[" .. thing .. "]")
      return res
   elseif "table" == t then
      return M.fastFromList(thing)
   elseif "pattern" == t then
      return thing
   else
      return pure(thing)
   end
end
M.reify = reify

local patternify = function(func)
   local patterned = function(...)
      local arity = nparams(func)
      local pats = map(reify, { ... })
      local pat = table.remove(pats, #pats)
      if arity == 1 then
         return func(pat)
      end
      local left = table.remove(pats, 1)
      local mapFn = function(...)
         local args = { ... }
         args[#args + 1] = pat
         return func(unpack(args))
      end
      mapFn = curry(mapFn, arity - 1)
      return reduce(appLeft, fmap(left, mapFn), pats):innerJoin()
   end
   return patterned
end

require "moon.all"
local function typecheck(f, name)
   local t = TYPES[name].T
   return function(...)
      local args = { ... }
      for i = 1, #args do
         local _t = t[i]
         if type(_t) == "table" then
            if _t.constructor then
               local tt = _t.constructor
               if tt == "Pattern" then
                  args[i] = reify(args[i])
               end
            end
         end
         if type(_t) == "string" then
            if _t == "Time" then
               args[i] = tofrac(args[i])
            end
         end
      end
      return f(unpack(args))
   end
end

local function register(args)
   local name, type_sig, f, should_pat = unpack(args)
   if type(should_pat) == "nil" then
      should_pat = true
   end
   if should_pat then
      TYPES[name] = TDef:new(type_sig)
      U[name] = f
      M[name] = auto_curry(nparams(f), patternify(f))
      base[name] = method_wrap(patternify(f))
   else
      -- local t = TDef:new(type_sig)
      TYPES[name] = TDef:new(type_sig)
      M[name] = typecheck(auto_curry(nparams(f), f), name)
      base[name] = method_wrap(f)
   end
end
M.register = register

function M.stack(pats)
   return reduce(M.overlay, silence(), iter(pats))
end

function M.stackFromList(list)
   return M.stack(map(pure, list))
end

---stack up pats in polymeter way
---@param steps number | Pattern
---@param pats Pattern
---@return Pattern
function M.polymeter(steps, pats)
   local nsteps, ratio, res = reify(steps), nil, {}
   for _, pat in iter(pats) do
      ratio = nsteps / #(firstCycle(pat))
      res[#res + 1] = M.fast(ratio, pat)
   end
   return M.stack(res)
end

function M.slowcat(pats)
   local query = function(_, state)
      local a = state.span
      local cyc = a._begin:sam():asFloat()
      local n = #pats
      local i = cyc % n
      local pat = pats[i + 1]
      if not pat then
         return {}
      end
      local offset = cyc - (cyc - i) / n
      return withEventTime(pat, function(t)
         return t + offset
      end):query(state:setSpan(a:withTime(function(t)
         return t - offset
      end)))
   end
   return Pattern(query):splitQueries()
end

function M.fromList(list)
   return M.slowcat(map(pure, list))
end

---Like slowcat, but the items are crammed into one cycle.
---@param pats Pattern<any>[]
---@return Pattern<any>
---@usage
---fastcat("e5", "b4", "d5", "c5") // "e5 b4 d5 c5"
function M.fastcat(pats)
   return U.fast(#pats, M.slowcat(pats))
end

function M.fastFromList(list)
   return M.fastcat(map(pure, list))
end

---@param args Pattern<any>[] | number[] #####
---@return Pattern<any>
function M.timecat(args)
   local total = 0
   for i, v in iter(args) do
      if i % 2 == 1 then
         total = total + v
      end
   end
   local accum = Fraction(0)
   local pats = {}
   local time, pat, b, e
   for i = 1, #args, 2 do
      time, pat = args[i], args[i + 1]
      b, e = accum / total, (accum + time) / total
      pats[#pats + 1] = M.compress(b, e, pat)
      accum = accum + time
   end
   return M.stack(pats)
end

function M.arrange(args)
   local total = 0
   for i, v in iter(args) do
      if i % 2 == 1 then
         total = total + v
      end
   end
   local cycles, pat
   for i = 1, #args, 2 do
      cycles, pat = args[i], args[i + 1]
      args[i + 1] = M.fast(cycles, pat)
   end
   return U.slow(total, M.timecat(args))
end

register {
   "overlay",
   "Pattern a -> Pattern a -> Pattern a",
   function(a, b)
      local query = function(_, st)
         return ut.concat(a:query(st), b:query(st))
      end
      return Pattern(query)
   end,
   false,
}

register {
   "superimpose",
   "(Pattern a -> Pattern a) -> Pattern a -> Pattern a",
   function(f, pat)
      return M.stack { pat, M.sl(f)(pat) }
   end,
   false,
}

-- register {
--    "layer",
--    nil,
--    function(tf, pat)
--       local acc = {}
--       for i, f in iter(tf) do
--          acc[i] = M.sl(f)(pat)
--       end
--       return M.stack(acc)
--    end,
--    false,
-- } -- diff with tidal -- TODO: "[Pattern a -> Pattern b] -> Pattern a -> Pattern b"

register {
   "fast",
   "Pattern Time -> Pattern a -> Pattern a",
   function(factor, pat)
      factor = tofrac(factor)
      if factor:eq(0) then
         return silence()
      elseif factor:lt(0) then
         return M.rev(U.fast(-factor, pat))
      else
         return withTime(pat, function(t)
            return t * factor
         end, function(t)
            return t / factor
         end)
      end
   end,
}

register {
   "slow",
   "Pattern Time -> Pattern a -> Pattern a",
   function(factor, pat)
      factor = tofrac(factor)
      if factor:eq(0) then
         return silence()
      else
         return M.fast(factor:reverse(), pat)
      end
   end,
}

-- rotL
register {
   "early",
   "Time -> Pattern a -> Pattern a",
   function(offset, pat)
      return withTime(pat, function(t)
         return t + offset
      end, function(t)
         return t - offset
      end)
   end,
   -- false,
} -- HACK: why not patternify TIME??

-- rotR
register {
   "late",
   "Time -> Pattern a -> Pattern a",
   function(offset, pat)
      return M.early(-offset, pat)
   end,
   -- false,
}

-- TODO: test
register {
   "inside",
   "Pattern Time -> (Pattern b -> Pattern a) -> Pattern b -> Pattern a",
   function(np, f, pat)
      local function _inside(n)
         return U.fast(n, f(U.slow(n, pat)))
      end
      return fmap(np, _inside):innerJoin()
   end,
   false,
}

register {
   "outside",
   "Pattern Time -> (Pattern b -> Pattern a) -> Pattern b -> Pattern a",
   function(factor, f, pat)
      return M.inside(1 / factor, f, pat)
   end,
   false,
}

register {
   "ply",
   "Pattern Time -> Pattern a -> Pattern a",
   function(n, pat)
      pat = fmap(pat, function(x)
         return U.fast(n, pure(x))
      end)
      return squeezeJoin(pat)
   end,
}

register {
   "fastgap",
   "Pattern Time -> Pattern a -> Pattern a",
   function(factor, pat)
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
      return Pattern(query):splitQueries()
   end,
}

register {
   "compress",
   "Time -> Time -> Pattern a -> Pattern a",
   function(b, e, pat)
      if b > e or e > Fraction(1) or b > Fraction(1) or b < Fraction(0) or e < Fraction(0) then
         return M.silence()
      end
      local fasted = U.fastgap((Fraction(1) / (e - b)), pat)
      return M.late(b, fasted)
   end,
   false,
}

register {
   "focus",
   "Time -> Time -> Pattern a -> Pattern a",
   function(b, e, pat)
      local fasted = U.fast((Fraction(1) / (e - b)), pat)
      return M.late(Span:cyclePos(b), fasted)
   end,
   false,
}

-- TODO: replace
M.focusSpan = function(span, pat)
   return M.focus(span._begin, span._end, reify(pat))
end

register {
   "zoom",
   "Time -> Time -> Pattern a -> Pattern a",
   function(s, e, pat)
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
      return splitQueries(withEventSpan(withQuerySpan(pat, qf), ef))
   end,
   false,
}

local _run = function(n)
   local list = fun.totable(fun.range(0, n - 1))
   return M.fastFromList(list)
end

register {
   "run",
   "Pattern Int -> Pattern Int",
   function(n)
      return fmap(n, _run):join()
   end,
   false,
}

local _scan = function(n)
   local res = {}
   for _, v in fun.range(1, n) do
      res[#res + 1] = M.run(v)
   end
   return M.slowcat(res)
end

register {
   "scan",
   "Pattern Int -> Pattern Int",
   function(n)
      return fmap(n, _scan):join()
   end,
   false,
}

local waveform = function(func)
   local query = function(_, state)
      return { Event(nil, state.span, func(state.span:midpoint())) }
   end
   return Pattern(query)
end

M.steady = function(value)
   return Pattern(function(state)
      return { Event(nil, state.span, value) }
   end)
end
local toBipolar = function(pat)
   return pat * 2 - 1
end

local fromBipolar = function(pat)
   return (pat + 1) / 2
end

M.sine2 = waveform(function(t)
   return sin(t:asFloat() * pi * 2)
end)
M.sine = fromBipolar(M.sine2)
M.cosine2 = M.late(1 / 4, M.sine2)
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
M.tri = M.fastcat { M.isaw, M.saw }
M.tri2 = M.fastcat { M.isaw2, M.saw2 }
M.time = waveform(id)
M.rand = waveform(timeToRand)

M._irand = function(i)
   return fmap(M.rand, function(x)
      return floor(x * i)
   end)
end
-- TODO: reigister
M.irand = function(ipat)
   return innerJoin(fmap(reify(ipat), M._irand))
end

local _chooseWith = function(pat, ...)
   local vals = { ... }
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

register {
   "degradeByWith",
   "Pattern Double -> Double -> Pattern a -> Pattern a",
   function(prand, by, pat)
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
   end,
   false,
}

register {
   "degradeBy",
   "Pattern Double -> Pattern a -> Pattern a",
   function(by, pat)
      return M.degradeByWith(M.rand, by, pat)
   end,
}

register {
   "undegradeBy",
   "Pattern Double -> Pattern a -> Pattern a",
   function(by, pat)
      return M.degradeByWith(
         fmap(M.rand, function(r)
            return 1 - r
         end),
         by,
         pat
      )
   end,
}

register {
   "degrade",
   "Pattern a -> Pattern a",
   function(pat)
      return U.degradeBy(0.5, pat)
   end,
}

register {
   "undegrade",
   "Pattern a -> Pattern a",
   function(pat)
      return U.undegradeBy(0.5, pat)
   end,
}

register {
   "sometimesBy",
   "Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a",
   function(by, func, pat)
      return innerJoin(fmap(by, function()
         return M.stack { U.degradeBy(by, pat), func(U.undegradeBy(1 - by, pat)) }
      end))
   end,
}

register {
   "sometimes",
   "(Pattern a -> Pattern a) -> Pattern a -> Pattern a",
   function(func, pat)
      return U.sometimesBy(0.5, func, pat)
   end,
}

-- TODO: ? register
function M.struct(boolpat, pat)
   local f = function(b)
      return function(val)
         return b and val or nil
      end
   end
   local bools = M.fastFromList(boolpat)
   return appLeft(fmap(bools, f), pat):removeNils()
end

register {
   "euclid",
   "Pattern Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a",
   function(n, k, offset, pat)
      return op["|?"](pat, bjork(n, k, offset))
   end,
}

register {
   "rev",
   "Pattern a -> Pattern a",
   function(pat)
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
   end,
}

register {
   "palindrome",
   "Pattern a -> Pattern a",
   function(pat)
      return M.slowcat { pat, U.rev(pat) }
   end,
}

register {
   "iter",
   "Pattern Int -> Pattern a -> Pattern a",
   function(n, pat)
      local acc = {}
      for i = 1, n do
         acc[i] = U.early(Fraction(i - 1, n), pat)
      end
      return M.fromList(acc)
   end,
}

register {
   "reviter",
   "Pattern Int -> Pattern a -> Pattern a",
   function(n, pat)
      local acc = {}
      for i = 1, n do
         acc[i] = M.late(Fraction(i - 1, n), pat)
      end
      return M.fromList(acc)
   end,
}

register {
   "segment",
   "Pattern Time -> Pattern a -> Pattern a",
   function(n, pat)
      return appLeft(M.fast(n, pure(id)), pat)
   end,
}

-- TODO: see tidal impl
register {
   "range",
   "Pattern a -> Pattern a -> Pattern a -> Pattern a",
   function(mi, ma, pat)
      return pat * (ma - mi) + mi
   end,
   false,
}

register {
   "off",
   "Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a",
   function(tp, f, pat)
      f = M.sl(f)
      local _off = function(t)
         return M.stack { pat, M.late(t, f(pat)) }
      end
      return fmap(tp, _off):innerJoin()
   end,
   false,
}

-- register("echoWith", function(times, time, func, pat)
--    local f = function(index)
--       return func(M.late(time * index, pat))
--    end
--    local ts
--    do
--       local _accum_0 = {}
--       local _len_0 = 1
--       for i = 0, times - 1 do
--          _accum_0[_len_0] = i
--          _len_0 = _len_0 + 1
--       end
--       ts = _accum_0
--    end
--    return M.stack(map(f, ts))
-- end)
--
register {
   "when",
   "(Int -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a",
   function(test, f, pat)
      local query = function(_, state)
         local cycle_idx = state.span._begin:sam():asFloat()
         if test(cycle_idx) then
            return f(pat):query(state)
         else
            return pat:query(state)
         end
      end
      return Pattern(query):splitQueries()
   end,
   false,
}

register {
   "every",
   "Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a",
   function(tp, f, pat)
      f = M.sl(f)
      local _every = function(t)
         return M.when(function(i)
            return i % t == 0
         end, f, pat)
      end
      return fmap(tp, _every):innerJoin()
   end,
   false,
}

register {
   "scale",
   "Pattern String -> Pattern a -> Pattern a",
   function(name, pat)
      local toScale = function(v)
         return getScale(name, v)
      end
      return fmap(pat, toScale)
   end,
}

register {
   "concat",
   "Pattern a -> Pattern a -> Pattern a",
   function(pat, other)
      return fmap(pat, function(a)
         return function(b)
            if type(a) == "table" then
               a[#a + 1] = b
               return a
            end
            return { a, b }
         end
      end):appLeft(other)
   end,
   false,
}

M.id = id
M.T = T
M.maxi = maxi
M.pipe = ut.pipe
M.dump = ut.dump2
M.u = U
M.t = TYPES
M.base = base

M.pp = function(a)
   print(M.dump(a))
end

return M
