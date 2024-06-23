local types = require "modal.types"
local Event, Span, State, Time = types.Event, types.Span, types.State, types.Time
local ut = require "modal.utils"
local fun = require "modal.fun"
local bjork = require "modal.euclid"
local getScale = require "modal.scales"
local TDef = require "modal.typedef"
local maxi = require "modal.maxi"

local unpack = unpack or rawget(table, "unpack")
local pairs = pairs
local ipairs = ipairs
local setmetatable = setmetatable
local iter = fun.iter
local reduce = fun.reduce
local tremove = table.remove
local str_format = string.format
local sin = math.sin
local min = math.min
local max = math.max
local pi = math.pi
local floor = math.floor
local id = ut.id
local map = ut.map
local filter = ut.filter
local dump = ut.dump
local flatten = ut.flatten
local curry = ut.curry
local union = ut.union
local concat = ut.concat
local flip = ut.flip
local method_wrap = ut.method_wrap
local curry_wrap = ut.curry_wrap
local nparams = ut.nparams
local get_args = ut.get_args
local timeToRand = ut.timeToRand
local T = ut.T

local fast, pure, fastcat, slowcat, stack, silence, focus, range

local M = {}
local TYPES = {}
local op = {}

local mini = maxi(M, false)
local sl = ut.string_lambda(M)

local function reify(thing)
   local t = T(thing)
   if "string" == t then
      local res = mini("[" .. thing .. "]")
      return res and res() or silence
   elseif "table" == t then
      return fastcat(thing)
   elseif "pattern" == t then
      return thing
   else
      return pure(thing)
   end
end
M.reify = reify

local mt = { __class = "pattern" }

function mt:len()
   return #(self(0, 1))
end

function mt:__call(b, e)
   return self:querySpan(b, e)
end

function mt:__tostring()
   return dump(self(0, 1))
end

function mt:show()
   return tostring(self)
end

-- TODO: not triggered in busted
function mt:__eq(other)
   return self:__tostring() == other:__tostring()
end

function mt:__concat(other)
   return op["|>"](self, other)
end

function mt:__add(other)
   return op["|+"](self, other)
end

function mt:__sub(other)
   return op["|-"](self, other)
end

function mt:__mul(other)
   return op["|*"](self, other)
end

function mt:__div(other)
   return op["|/"](self, other)
end

function mt:__mod(other)
   return op["|%"](self, other)
end

function mt:__pow(other)
   return op["|^"](self, other)
end

function mt:slowcat(pats)
   pats[#pats + 1] = self
   return slowcat(pats)
end

-- TODO: intuitive??
function mt:fastcat(pats)
   pats[#pats + 1] = self
   return fastcat(pats)
end

function mt:stack(pats)
   pats[#pats + 1] = self
   return stack(pats)
end

mt.__index = mt

-- automatically export pattern methods
setmetatable(mt, {
   __newindex = function(_, k, v)
      M[k] = v
   end,
   __index = function(_, k)
      return M[k]
   end,
})

---@class Pattern
local function Pattern(query)
   query = query or function()
      return {}
   end
   return setmetatable({ query = query }, mt)
end
M.Pattern = Pattern

local function querySpan(pat, b, e)
   local span = Span(b, e)
   local state = State(span)
   return setmetatable(pat.query(state), {
      __tostring = function(self)
         return dump(self)
      end,
   })
end
mt.querySpan = querySpan

local function filterEvents(pat, func)
   local query = function(state)
      local events = pat.query(state)
      return filter(func, events)
   end
   return Pattern(query)
end
mt.filterEvents = filterEvents

local function filterValues(pat, condf)
   local query = function(state)
      local events = pat.query(state)
      local f = function(event)
         return condf(event.value)
      end
      return filter(f, events)
   end
   return Pattern(query)
end
mt.filterValues = filterValues

local function removeNils(pat)
   return pat:filterValues(function(v)
      return v ~= nil
   end)
end
mt.removeNils = removeNils

local function splitQueries(pat)
   local query = function(state)
      local cycles = state.span:spanCycles()
      local f = function(subspan)
         return pat.query(state:setSpan(subspan))
      end
      -- TODO: replace
      return flatten(map(f, cycles))
   end
   return Pattern(query)
end
mt.splitQueries = splitQueries

local function withValue(pat, func)
   local query = function(state)
      local events = pat.query(state)
      local f = function(event)
         return event:withValue(func)
      end
      return map(f, events)
   end
   return Pattern(query)
end
mt.withValue = withValue

local fmap = withValue
mt.fmap = fmap

local function withQuerySpan(pat, func)
   local query = function(state)
      local new_state = state:withSpan(func)
      return pat.query(new_state)
   end
   return Pattern(query)
end
mt.withQuerySpan = withQuerySpan

local function withQueryTime(pat, func)
   return withQuerySpan(pat, function(span)
      return span:withTime(func)
   end)
end
mt.withQueryTime = withQueryTime

local function withEvents(pat, func)
   return Pattern(function(state)
      return func(pat.query(state))
   end)
end
mt.withEvents = withEvents

local function withEvent(pat, func)
   return withEvents(pat, function(events)
      return map(func, events)
   end)
end
mt.withEvent = withEvent

local function withEventSpan(pat, func)
   local query = function(state)
      local events = pat.query(state)
      return map(function(ev)
         return ev:withSpan(func)
      end, events)
   end
   return Pattern(query)
end
mt.withEventSpan = withEventSpan

local function withEventTime(pat, func)
   local query = function(state)
      local events = pat.query(state)
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
mt.withEventTime = withEventTime

local function withTime(pat, qf, ef)
   local query = withQueryTime(pat, qf)
   local pattern = withEventTime(query, ef)
   return pattern
end
mt.withTime = withTime

local function onsetsOnly(pat)
   return filterEvents(pat, function(event)
      return event:hasOnset()
   end)
end
mt.onsetsOnly = onsetsOnly

local function discreteOnly(pat)
   return filterEvents(pat, function(event)
      return event.whole
   end)
end
mt.discreteOnly = discreteOnly

local function appWhole(pat, whole_func, pat_val)
   local query = function(state)
      local event_funcs = pat.query(state)
      local event_vals = pat_val.query(state)
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
mt.appWhole = appWhole

-- Tidal's <*>
local function appBoth(pat, pat_val)
   local whole_func = function(span_a, span_b)
      if not span_a or not span_b then
         return
      end
      return span_a:sect(span_b)
   end
   return appWhole(pat, whole_func, pat_val)
end
mt.appBoth = appBoth

-- Tidal's <*
local function appLeft(pat, pat_val)
   local query = function(state)
      local events = {}
      local event_funcs = pat.query(state)
      for _, event_func in ipairs(event_funcs) do
         local whole = event_func:wholeOrPart()
         local event_vals = pat_val.query(state:setSpan(whole))
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
mt.appLeft = appLeft

-- Tidal's *>
local function appRight(pat, pat_val)
   local query = function(state)
      local events = {}
      local event_vals = pat_val.query(state)
      for _, event_val in ipairs(event_vals) do
         local whole = event_val:wholeOrPart()
         local event_funcs = pat.query(state:setSpan(whole))
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
mt.appRight = appRight

local function bindWhole(pat, choose_whole, func)
   local query = function(state)
      local withWhole = function(a, b)
         local new_whole = choose_whole(a.whole, b.whole)
         return Event(new_whole, b.part, b.value)
      end
      local match = function(a)
         local events = func(a.value).query(state:setSpan(a.part))
         local f = function(b)
            return withWhole(a, b)
         end
         return map(f, events)
      end
      local events = pat.query(state)
      return flatten(map(match, events))
   end
   return Pattern(query)
end
mt.bindWhole = bindWhole

local function bind(pat, func)
   local whole_func = function(a, b)
      if a == nil or b == nil then
         return nil
      end
      return a:sect(b)
   end
   return bindWhole(pat, whole_func, func)
end
mt.bind = bind

local function join(pat)
   return bind(pat, id)
end
mt.join = join

local function outerBind(pat, func)
   return bindWhole(pat, function(a)
      return a
   end, func)
end
mt.outerBind = outerBind

local function innerBind(pat, func)
   return bindWhole(pat, function(_, b)
      return b
   end, func)
end
mt.innerBind = innerBind

local function outerJoin(pat)
   return outerBind(pat, id)
end
mt.outerJoin = outerJoin

local function innerJoin(pat)
   return innerBind(pat, id)
end
mt.innerJoin = innerJoin

local function squeezeJoin(pat)
   local query = function(state)
      local events = discreteOnly(pat).query(state)
      local flatEvent = function(outerEvent)
         local span = outerEvent:wholeOrPart()
         local innerPat = M.focus(span._begin, span._end, outerEvent.value)
         local innerEvents = innerPat.query(state:setSpan(outerEvent.part))
         local munge = function(outer, inner)
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
         local f = function(innerEvent)
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
mt.squeezeJoin = squeezeJoin

local function squeezeBind(pat, func)
   return squeezeJoin(fmap(pat, func))
end
mt.squeezeBind = squeezeBind

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
   uni = function (a, b) return union(a, b) end, -- TODO: use local
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

for k, f in pairs(ops) do
   op[k] = {}
   for _, v in ipairs(hows) do
      op[k][v] = _op[v](f)
      if op_set[k] and how_format[v] then
         local symb = str_format(how_format[v], op_set[k])
         op[symb] = _op[v](f)
      end
   end
end
op["#"] = op["|>"]

M.op = op

---@type Pattern
silence = Pattern()
M.silence = silence

function pure(value)
   local query = function(state)
      local cycles = state.span:spanCycles()
      local f = function(span)
         local whole = span._begin:wholeCycle()
         return Event(whole, span, value)
      end
      for i, v in iter(cycles) do
         cycles[i] = f(v)
      end
      return cycles
   end
   return Pattern(query)
end
M.pure = pure

local function purify(value)
   if T(value) == "pattern" then
      return value
   else
      return pure(value)
   end
end

local function patternify(func)
   return function(...)
      local arity = nparams(func)
      local pats = { ... }
      local pat = tremove(pats, #pats)
      if arity == 1 then
         return func(pat)
      end
      local left = tremove(pats, 1)
      local mapFn = function(...)
         local args = { ... }
         args[#args + 1] = pat
         return func(unpack(args))
      end
      mapFn = curry(mapFn, arity - 1)
      return innerJoin(reduce(appLeft, fmap(left, mapFn), pats))
   end
end

local function type_wrap(f, name)
   local sig = TYPES[name]
   return function(...)
      local args = { ... }
      for i, v in ipairs(args) do
         local t = sig[i]
         local tc, tvar, istable = t.constructor, t[1], t.istable
         if istable then
            for j, vv in ipairs(v) do
               if tc then
                  if tc == "Pattern" then
                     v[j] = purify(vv) -- for fastcat and slowcat ...
                  end
               end
            end
         else
            if T(tvar) == "table" or tvar == "f" then
               v = sl(v)
            end
            if tvar == "Time" then
               v = Time(v)
            end
            if tc then
               if tc == "Pattern" then
                  v = reify(v)
               end
            end
            args[i] = v
         end
      end
      return f(unpack(args))
   end
end

local function register(type_sig, f, nify)
   local tdef, name = TDef(type_sig)
   if T(nify) == "nil" then
      nify = true
   end
   local arity = nparams(f)
   local arg_names = get_args(f)
   for i, v in pairs(arg_names) do
      tdef[i].name = v
   end
   if nify then
      TYPES[name] = tdef
      local f_p = patternify(f)
      local f_p_t = type_wrap(f_p, name)
      local f_c_p_t = curry_wrap(arity, f_p_t)
      M[name] = f_c_p_t
      rawset(mt, name, method_wrap(f_c_p_t))
   else
      TYPES[name] = TDef(type_sig)
      local f_t = type_wrap(f, name)
      local f_t_c = curry_wrap(arity, f_t)
      M[name] = f_t_c
      rawset(mt, name, method_wrap(f_t))
   end
end
M.register = register

---stack two patterns
---@param a any
---@param b any
---@return Pattern
local function overlay(a, b)
   local query = function(st)
      return concat(a.query(st), b.query(st))
   end
   return Pattern(query)
end
register("overlay :: Pattern a -> Pattern a -> Pattern a", overlay, false)

---stack a table of patterns
---@param pats table<any>
---@return Pattern
function stack(pats)
   return reduce(overlay, silence, iter(pats))
end
register("stack :: [Pattern a] -> Pattern a", stack, false)

---aligns one or more given sequences to the given number of steps per cycle.
---@param steps number
---@param pats table
---@return Pattern
function M.polymeter(steps, pats)
   local res = {}
   for i, pat in iter(pats) do
      res[i] = fast(steps / pat:len(), pat)
   end
   return stack(res)
end
-- register("polymeter :: Pattern Int -> [Pattern a] -> Pattern a", polymeter, false)

function slowcat(pats)
   local query = function(state)
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
      end).query(state:setSpan(a:withTime(function(t)
         return t - offset
      end)))
   end
   return Pattern(query):splitQueries()
end
register("slowcat :: [Pattern a] -> Pattern a", slowcat, false)

function fastcat(pats)
   return fast(#pats, M.slowcat(pats))
end
register("fastcat :: [Pattern a] -> Pattern a", fastcat, false)

function M.timecat(tups)
   local total = 0
   for i, v in iter(tups) do
      if i % 2 == 1 then
         total = total + v
      end
   end
   local accum = 0
   local pats = {}
   local time, pat, b, e
   for i = 1, #tups, 2 do
      time, pat = tups[i], tups[i + 1]
      b, e = accum / total, (accum + time) / total
      pats[#pats + 1] = M.compress(b, e, pat)
      accum = accum + time
   end
   return stack(pats)
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
   return slow(total, M.timecat(args))
end

local function superimpose(f, pat)
   return overlay(pat, f(pat))
end
register("superimpose :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a", superimpose, false)

local function layer(tf, pat)
   local acc = {}
   for i, f in iter(tf) do
      acc[i] = f(pat)
   end
   return stack(acc)
end
register("layer :: [(Pattern a -> Pattern b)] -> Pattern a -> Pattern b", layer) -- a little ugly lol layer, false,

function fast(factor, pat)
   factor = Time(factor) -- TODO:
   if factor:eq(0) then
      return silence
   elseif factor:lt(0) then
      return M.rev(fast(-factor, pat))
   else
      return withTime(pat, function(t)
         return t * factor
      end, function(t)
         return t / factor
      end)
   end
end
register("fast :: Pattern Time -> Pattern a -> Pattern a", fast)

local function slow(factor, pat)
   -- factor = Time(factor)
   if factor:eq(0) then
      return silence
   else
      return fast(factor:reverse(), pat)
   end
end

register("slow :: Pattern Time -> Pattern a -> Pattern a", slow)

-- rotL
local function early(offset, pat)
   return withTime(pat, function(t)
      return t + offset
   end, function(t)
      return t - offset
   end)
end
register("early :: Time -> Pattern a -> Pattern a", early, false) -- HACK: why not patternify TIME??

-- rotR
local function late(offset, pat)
   return M.early(-offset, pat)
end
register("late :: Time -> Pattern a -> Pattern a", late, false)

local function inside(np, f, pat)
   local function _inside(n)
      return fast(n, f(slow(n, pat)))
   end
   return fmap(np, _inside):innerJoin()
end
register("inside :: Pattern Time -> (Pattern b -> Pattern a) -> Pattern b -> Pattern a", inside, false)

local function outside(factor, f, pat)
   return inside(1 / factor, f, pat)
end
register("outside :: Pattern Time -> (Pattern b -> Pattern a) -> Pattern b -> Pattern a", outside, false)

---repeats each event the given number of times.
---@param n number
---@param pat any
---@return Pattern
local function ply(n, pat)
   pat = fmap(pat, function(x)
      return fast(n, pure(x))
   end)
   return squeezeJoin(pat)
end
register("ply :: Pattern Time -> Pattern a -> Pattern a", ply)

---speeds up a pattern like fast, but rather than it playing multiple times as fast would it instead leaves a gap in the remaining space of the cycle. For example, the following will play the sound pattern "bd sn" only once but compressed into the first half of the cycle, i.e. twice as fast.
---@param factor number
---@param pat any
---@return Pattern
local function fastgap(factor, pat)
   factor = Time(factor)
   if factor <= Time(0) then
      return silence
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
   local query = function(state)
      local span = state.span
      local new_span = Span(mungeQuery(span._begin), mungeQuery(span._end))
      if new_span._begin == new_span._begin:nextSam() then
         return {}
      end
      local new_state = State(new_span)
      local events = pat.query(new_state)
      local f = function(event)
         return event:withSpan(eventSpanFunc)
      end
      return map(f, events)
   end
   return Pattern(query):splitQueries()
end
register("fastgap :: Pattern Time -> Pattern a -> Pattern a", fastgap)

---Compress each cycle into the given timespan, leaving a gap
---@param b Time
---@param e Time
---@param pat any
---@return Pattern
local function compress(b, e, pat)
   if b > e or e > Time(1) or b > Time(1) or b < Time(0) or e < Time(0) then
      return silence
   end
   local fasted = fastgap((e - b):reverse(), pat)
   return M.late(b, fasted)
end
register("compress :: Time -> Time -> Pattern a -> Pattern a", compress, false)

---similar to `compress`, but doesn't leave gaps, and the 'focus' can be bigger than a cycle
---@param b Time
---@param e Time
---@param pat any
---@return Pattern
function focus(b, e, pat)
   local fasted = fast((e - b):reverse(), pat)
   return late(b:cyclePos(), fasted)
end
register("focus :: Time -> Time -> Pattern a -> Pattern a", focus, false)

local function zoom(s, e, pat)
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
   return withEventSpan(withQuerySpan(pat, qf), ef):splitQueries()
end
register("zoom :: Time -> Time -> Pattern a -> Pattern a", zoom, false)

local _run = function(n)
   local list = fun.totable(fun.range(0, n - 1))
   return fastcat(list)
end

---generate 1 .. n fastcated
---@param n any
---@return Pattern
local function run(n)
   return fmap(n, _run):join()
end
register("run :: Pattern Int -> Pattern Int", run, false)

local _scan = function(n)
   local res = {}
   for _, v in fun.range(1, n) do
      res[#res + 1] = run(v)
   end
   return slowcat(res)
end

local function scan(n)
   return fmap(n, _scan):join()
end
register("scan :: Pattern Int -> Pattern Int", scan, false)

local waveform = function(func)
   local query = function(state)
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
M.cosine2 = late(1 / 4, M.sine2)
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
M.tri = fastcat { M.isaw, M.saw }
M.tri2 = fastcat { M.isaw2, M.saw2 }
M.time = waveform(id)
M.rand = waveform(timeToRand)

local _irand = function(i)
   return fmap(M.rand, function(x)
      return floor(x * i)
   end)
end
--@randrun n@ generates a pattern of random integers less than @n@.
-- TODO: use in maxi?
-- TODO: register
local irand = function(ipat)
   return fmap(reify(ipat), _irand):innerJoin()
end
register("irand :: Pattern Num -> Pattern Num", irand)

local _chooseWith = function(pat, ...)
   local vals = { ... }
   if #vals == 0 then
      return silence
   end
   return fmap(range(1, #vals + 1, pat), function(i)
      local key = min(max(floor(i), 0), #vals)
      return vals[key]
   end)
end

-- local chooseWith = function(pat, ...)
--    return _chooseWith(pat, ...):outerJoin()
-- end

local chooseInWith = function(pat, ...)
   return _chooseWith(pat, ...):innerJoin()
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

local function degradeByWith(prand, by, pat)
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
      prand:filterValues(f)
   )
end

register("degradeByWith :: Pattern Double -> Double -> Pattern a -> Pattern a", degradeByWith, false)

local function degradeBy(by, pat)
   return degradeByWith(M.rand, by, pat)
end
register("degradeBy :: Pattern Double -> Pattern a -> Pattern a", degradeBy)

local function undegradeBy(by, pat)
   return degradeByWith(
      fmap(M.rand, function(r)
         return 1 - r
      end),
      by,
      pat
   )
end
register("undegradeBy :: Pattern Double -> Pattern a -> Pattern a", undegradeBy)

local function degrade(pat)
   return degradeBy(0.5, pat)
end
register("degrade :: Pattern a -> Pattern a", degrade)

local function undegrade(pat)
   return undegradeBy(0.5, pat)
end
register("undegrade :: Pattern a -> Pattern a", undegrade)

local function sometimesBy(by, func, pat)
   return fmap(by, function()
      return overlay(degradeBy(by, pat), func(undegradeBy(1 - by, pat)))
   end):innerJoin()
end
register("sometimesBy :: Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a", sometimesBy)

local function sometimes(func, pat)
   return sometimesBy(0.5, func, pat)
end
register("sometimes :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a", sometimes)

---applies the given structure to the pattern, alias to op.keepif.Out
---@param boolpat table<boolean>
---@param pat any
---@return Pattern
local function struct(boolpat, pat)
   return op.keepif.Out(pat, boolpat)
end
register("struct :: [Pattern bool] -> Pattern a -> Pattern a", struct, false)

local function euclid(n, k, pat)
   return struct(bjork(n, k, 0), pat)
end
register("euclid :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a", euclid)

local function euclidRot(n, k, rot, pat)
   return struct(bjork(n, k, rot), pat)
end
register("euclidRot :: Pattern Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a", euclidRot)

---reverse pattern in every cycle
---@param pat any
---@return Pattern
local function rev(pat)
   local query = function(state)
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
      local events = pat.query(state:setSpan(reflect(span)))
      return map(function(event)
         return event:withSpan(reflect)
      end, events)
   end
   return Pattern(query)
end
register("rev :: Pattern a -> Pattern a", rev)

local function iter_(n, pat)
   local acc = {}
   for i = 1, n do
      acc[i] = early((i - 1) / n, pat)
   end
   return slowcat(acc)
end
register("iter :: Pattern Int -> Pattern a -> Pattern a", iter_)

local function reviter(n, pat)
   local acc = {}
   for i = 1, n do
      acc[i] = late((i - 1) / n, pat)
   end
   return slowcat(acc)
end
register("reviter :: Pattern Int -> Pattern a -> Pattern a", reviter)

local function segment(n, pat)
   return appLeft(M.fast(n, id), pat)
end
register("segment :: Pattern Time -> Pattern a -> Pattern a", segment)

---limit value in pattern to a range TODO: see tidal impl
---@param mi number
---@param ma number
---@param pat number
---@return unknown
function range(mi, ma, pat)
   return pat * (ma - mi) + mi
end
register("range :: Pattern number -> Pattern number -> Pattern number -> Pattern a", range, false)

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
--    return stack(map(f, ts))
-- end)
--
local function when(test, f, pat)
   local query = function(state)
      local cycle_idx = state.span._begin:sam():asFloat()
      if test(cycle_idx) then
         return (f(pat)).query(state)
      else
         return pat.query(state)
      end
   end
   return Pattern(query):splitQueries()
end
register("when :: (Int -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a", when, false)

local slowcatPrime = function(pats)
   local query = function(state)
      local len = #pats
      local index = state.span._begin:sam():asFloat() % len + 1
      local pat = pats[index]
      return pat.query(state)
   end
   return Pattern(query):splitQueries()
end

local function every(n, f, pat)
   local acc = {}
   for i = 1, n do
      acc[i] = (i == 1) and f(pat) or pat
   end
   return slowcatPrime(acc)
end
-- HACK: "Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a",
register("every :: Pattern Int -> Pattern f -> Pattern a -> Pattern a", every)

local function off(tp, f, pat)
   return pat:overlay(f(pat:late(tp)))
end
register("off :: Pattern Time -> Pattern f -> Pattern a -> Pattern a", off)

local function scale(name, pat)
   return fmap(pat, getScale(name))
end
-- TODO: "Pattern String -> Pattern a -> Pattern a",
register("scale :: String -> Pattern a -> Pattern a", scale, false)

---union two value maps .. bit weird ...
---@param pat ValueMap
---@param other ValueMap
---@return Pattern
local function chain(pat, other)
   return fmap(pat, function(a)
      return function(b)
         if T(a) == "table" then
            a[#a + 1] = b
            return a
         end
         return { a, b }
      end
   end):appLeft(other)
end
register("chain :: Pattern ValueMap -> Pattern ValueMap -> Pattern ValueMap", chain, false)

M.id = id
M.T = T
M.maxi = maxi
M.pipe = ut.pipe
M.dump = ut.dump
M.t = TYPES
M.mt = mt
M.mini = mini
M.sl = sl

return M
