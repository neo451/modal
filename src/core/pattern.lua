local types = require "modal.types"
local Event, Span, State, Time, TDef = types.Event, types.Span, types.State, types.Time, types.TDef
local ut = require "modal.utils"
local bjork = require "modal.euclid"
local getScale = require "modal.scales"

local unpack = unpack or rawget(table, "unpack")
local pairs = pairs
local ipairs = ipairs
local setmetatable = setmetatable
local tconcat = table.concat
local tremove = table.remove
local str_format = string.format
local sin = math.sin
local min = math.min
local max = math.max
local pi = math.pi
local floor = math.floor
local reduce = ut.reduce
local map = ut.map
local id = ut.id
local filter = ut.filter
local dump = ut.dump
local curry = ut.curry
local union = ut.union
local concat = ut.concat
local flip = ut.flip
local method_wrap = ut.method_wrap
local curry_wrap = ut.curry_wrap
local nparams = ut.nparams
local get_args = ut.get_args
local timeToRand = ut.timeToRand
local memoize = ut.memoize
local T = ut.T

local fast, pure, fastcat, slowcat, stack, silence, focus, range, rev, compress

local M = {}
local TYPES = {}
local op = {}

-- give mini access to global vars
setmetatable(M, { __index = _G })

local mini = require("modal.maxi").mini(M)
local reify = memoize(function(thing)
   local t = T(thing)
   if "string" == t then
      local res = mini(thing)
      return res and res or silence
   elseif "table" == t then
      return fastcat(thing)
   elseif "pattern" == t then
      return thing
   else
      return pure(thing)
   end
end)
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
   return filterValues(pat, function(v)
      return v ~= nil
   end)
end
mt.removeNils = removeNils

local function splitQueries(pat)
   local query = function(state)
      local cycles = state.span:spanCycles()
      local res = {}
      for i = 1, #cycles do
         local evs = pat.query(state:setSpan(cycles[i]))
         for j = 1, #evs do
            res[#res + 1] = evs[j]
         end
      end
      return res
   end
   return Pattern(query)
end
mt.splitQueries = splitQueries

local function withValue(pat, f)
   local query = function(state)
      local events = pat.query(state)
      for i = 1, #events do
         events[i] = events[i]:withValue(f)
      end
      return events
   end
   return Pattern(query)
end
mt.withValue = withValue

local fmap = withValue
mt.fmap = fmap

local function withQuerySpan(pat, f)
   local query = function(state)
      local new_state = state:withSpan(f)
      return pat.query(new_state)
   end
   return Pattern(query)
end
mt.withQuerySpan = withQuerySpan

local function withQueryTime(pat, f)
   return withQuerySpan(pat, function(span)
      return span:withTime(f)
   end)
end
mt.withQueryTime = withQueryTime

local function withEvents(pat, f)
   return Pattern(function(state)
      return f(pat.query(state))
   end)
end
mt.withEvents = withEvents

local function withEvent(pat, f)
   return withEvents(pat, function(events)
      for i = 1, #events do
         events[i] = f(events[i])
      end
      return events
   end)
end
mt.withEvent = withEvent

local function withEventSpan(pat, f)
   local query = function(state)
      local events = pat.query(state)
      for i = 1, #events do
         events[i] = events[i]:withSpan(f)
      end
      return events
   end
   return Pattern(query)
end
mt.withEventSpan = withEventSpan

local function withEventTime(pat, f)
   local query = function(state)
      local events = pat.query(state)
      local time_func = function(span)
         return span:withTime(f)
      end
      local event_func = function(event)
         return event:withSpan(time_func)
      end
      for i = 1, #events do
         events[i] = event_func(events[i])
      end
      return events
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
      local events = pat.query(state)
      local res = {}
      for _, a in ipairs(events) do
         local evs = func(a.value).query(state:setSpan(a.part))
         for _, b in ipairs(evs) do
            res[#res + 1] = Event(choose_whole(a.whole, b.whole), b.part, b.value)
         end
      end
      return res
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
   return bindWhole(pat, function(a, _)
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
         for i = 1, #innerEvents do
            innerEvents[i] = munge(outerEvent, innerEvents[i])
         end
         return innerEvents
      end
      local result = {}
      for i = 1, #events do
         local evs = flatEvent(events[i])
         for j = 1, #evs do
            result[#result + 1] = evs[j]
         end
      end
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

silence = Pattern()
M.silence = silence

function pure(value)
   local query = function(state)
      local cycles = state.span:spanCycles()
      for i, v in ipairs(cycles) do
         cycles[i] = Event(v._begin:wholeCycle(), v, value)
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
            if tvar == "Time" then
               v = Time(v)
            end
            if tc then
               if tc == "Pattern" and tvar == "f" and type(v) == "string" then
                  v = reify("(" .. v .. ")")
               elseif tc == "Pattern" then
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
   local arg_names = get_args(f)
   local arity = #arg_names
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
      TYPES[name] = tdef
      local f_t = type_wrap(f, name)
      local f_t_c = curry_wrap(arity, f_t)
      M[name] = f_t_c
      rawset(mt, name, method_wrap(f_t))
   end
end
M.register = register

local function overlay(a, b)
   local query = function(st)
      return concat(a.query(st), b.query(st))
   end
   return Pattern(query)
end
register("overlay :: Pattern a -> Pattern a -> Pattern a", overlay, false)

function stack(pats)
   return reduce(overlay, silence, pats)
end
register("stack :: [Pattern a] -> Pattern a", stack, false)

function M.polymeter(steps, pats)
   for i, pat in ipairs(pats) do
      pats[i] = fast(steps / pat:len(), pat)
   end
   return stack(pats)
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
   return splitQueries(Pattern(query))
end
register("slowcat :: [Pattern a] -> Pattern a", slowcat, false)

function fastcat(pats)
   return fast(#pats, M.slowcat(pats))
end
register("fastcat :: [Pattern a] -> Pattern a", fastcat, false)

local function timecat(tups)
   local total = 0
   for i, v in ipairs(tups) do
      if i % 2 == 1 then
         total = total + v
      end
   end
   local accum = Time(0)
   local pats = {}
   local time, pat, b, e
   for i = 1, #tups, 2 do
      time, pat = tups[i], reify(tups[i + 1])
      b, e = accum / total, (accum + time) / total
      pats[#pats + 1] = compress(b, e, pat)
      accum = accum + time
   end
   return stack(pats)
end
M.timecat = timecat

local function arrange(tups)
   local total = 0
   for i, v in ipairs(tups) do
      if i % 2 == 1 then
         total = total + v
      end
   end
   local cycles, pat
   for i = 1, #tups, 2 do
      cycles, pat = tups[i], reify(tups[i + 1])
      tups[i + 1] = fast(cycles, pat)
   end
   return slow(total, timecat(tups))
end
M.arrange = arrange

local function superimpose(f, pat)
   return overlay(pat, f(pat))
end
register("superimpose :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a", superimpose, false)

local function layer(tf, pat)
   for i, f in ipairs(tf) do
      tf[i] = f(pat)
   end
   return stack(tf)
end
register("layer :: [(Pattern a -> Pattern b)] -> Pattern a -> Pattern b", layer, false) -- a little ugly lol layer

function fast(factor, pat)
   factor = Time(factor) -- TODO:
   if factor:eq(0) then
      return silence
   elseif factor:lt(0) then
      return rev(fast(-factor, pat))
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
   return early(-offset, pat)
end
register("late :: Time -> Pattern a -> Pattern a", late, false)

local function inside(np, f, pat)
   local function _inside(n)
      return fast(n, f(slow(n, pat)))
   end
   return innerJoin(fmap(np, _inside))
end
register("inside :: Pattern Time -> (Pattern b -> Pattern a) -> Pattern b -> Pattern a", inside, false)

local function outside(factor, f, pat)
   return inside(1 / factor, f, pat)
end
register("outside :: Pattern Time -> (Pattern b -> Pattern a) -> Pattern b -> Pattern a", outside, false)

local function ply(n, pat)
   pat = fmap(pat, function(x)
      return fast(n, pure(x))
   end)
   return squeezeJoin(pat)
end
register("ply :: Pattern Time -> Pattern a -> Pattern a", ply)

local function fastgap(factor, pat)
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
      for i = 1, #events do
         events[i] = events[i]:withSpan(eventSpanFunc)
      end
      return events
   end
   return splitQueries(Pattern(query))
end
register("fastgap :: Pattern Time -> Pattern a -> Pattern a", fastgap)

function compress(b, e, pat)
   if b > e or e > Time(1) or b > Time(1) or b < Time(0) or e < Time(0) then
      return silence
   end
   local fasted = fastgap((e - b):reverse(), pat)
   return late(b, fasted)
end
register("compress :: Time -> Time -> Pattern a -> Pattern a", compress, false)

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
   return splitQueries(withEventSpan(withQuerySpan(pat, qf), ef))
end
register("zoom :: Time -> Time -> Pattern a -> Pattern a", zoom, false)

local _run = function(n)
   local list = {}
   for i = 1, n do
      list[i] = i - 1
   end
   return fastcat(list)
end

local function run(n)
   return join(fmap(n, _run))
end
register("run :: Pattern Int -> Pattern Int", run, false)

local _scan = function(n)
   local res = {}
   for i = 1, n do
      res[i] = run(pure(i))
   end
   return slowcat(res)
end

local function scan(n)
   return join(fmap(n, _scan))
end
register("scan :: Pattern Int -> Pattern Int", scan, false)

local waveform = function(func)
   local query = function(state)
      return { Event(nil, state.span, func(state.span:midpoint())) }
   end
   return Pattern(query)
end

local function segment(n, pat)
   return appLeft(fast(n, pure(id)), pat)
end
register("segment :: Pattern Time -> Pattern a -> Pattern a", segment)

function range(mi, ma, pat)
   return pat * (ma - mi) + mi
end

register("range :: Pattern number -> Pattern number -> Pattern number -> Pattern a", range)

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

-- TODO: use in maxi?
local irand = function(ipat)
   return innerJoin(fmap(ipat, _irand))
end
register("irand :: Pattern Num -> Pattern Num", irand)

local _chooseWith = function(pat, vals)
   if #vals == 0 then
      return silence
   end
   return fmap(range(1, #vals + 1, pat), function(i)
      local key = min(max(floor(i), 0), #vals)
      return vals[key]
   end)
end

local chooseWith = function(pat, ...)
   return _chooseWith(pat, ...):outerJoin()
end

local chooseInWith = function(pat, vals)
   return innerJoin(_chooseWith(pat, vals))
end

local choose = function(vals)
   return chooseInWith(M.rand, vals)
end

local randcat = function(pats)
   return segment(1, choose(pats))
end
register("randcat :: [Pattern a] -> Pattern a", randcat, false)

local function degradeByWith(prand, by, pat)
   if T(by) == "time" then
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
end

register("degradeByWith :: Pattern Double -> Double -> Pattern a -> Pattern a", degradeByWith)

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
   local f = function()
      return overlay(degradeBy(by, pat), func(undegradeBy(1 - by, pat)))
   end
   return innerJoin(fmap(by, f))
end
register("sometimesBy :: Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a", sometimesBy)

local function sometimes(func, pat)
   return sometimesBy(0.5, func, pat)
end
register("sometimes :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a", sometimes)

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

function rev(pat)
   local query = function(state)
      local span = state.span
      local cycle = span._begin:sam()
      local nextCycle = span._begin:nextSam()
      local reflect = function(to_reflect)
         local reflected = to_reflect:withTime(function(t)
            return cycle + (nextCycle - t)
         end)
         local tmp = reflected._begin
         reflected._begin = reflected._end
         reflected._end = tmp
         return reflected
      end
      local events = pat.query(state:setSpan(reflect(span)))
      for i = 1, #events do
         events[i] = events[i]:withSpan(reflect)
      end
      return events
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

--- TODO:
local function echoWith(times, time, f, pat)
   local acc = {}
   for i = 0, times - 1 do
      acc[i] = f(M.late(time * i, pat))
   end
   return stack(acc)
end
register("echoWith :: Pattern Int -> Pattern Int -> Pattern f -> Pattern a -> Pattern a", echoWith)

local function when(test, f, pat)
   local query = function(state)
      local cycle_idx = state.span._begin:sam()
      if test(cycle_idx) then
         return f(pat).query(state)
      else
         return pat.query(state)
      end
   end
   return splitQueries(Pattern(query))
end
register("when :: (Int -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a", when)

local slowcatPrime = function(pats)
   local query = function(state)
      local index = state.span._begin:sam():asFloat() % #pats + 1
      local pat = pats[index]
      return pat.query(state)
   end
   return splitQueries(Pattern(query))
end

local function every(n, f, pat)
   local acc = {}
   for i = 1, n do
      acc[i] = (i == 1) and f(pat) or pat
   end
   return slowcatPrime(acc)
end
-- nicer to write than f as ( -> ), just reify f
-- register("every :: Pattern Int -> Pattern (a -> a) -> Pattern a -> Pattern a", every)
register("every :: Pattern Int -> Pattern f -> Pattern a -> Pattern a", every)

local function off(tp, f, pat)
   return overlay(f(late(tp, pat)), pat)
end
-- HACK:
register("off :: Pattern Time -> Pattern b -> Pattern a -> Pattern a", off)

local function scale(name, pat)
   return fmap(pat, getScale(name))
end
-- TODO: "Pattern String -> Pattern a -> Pattern a",
register("scale :: String -> Pattern a -> Pattern a", scale, false)

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

local gcd_reduce = function(tab)
   return reduce(function(acc, value)
      return acc:gcd(value)
   end, tab[1], tab)
end

local function drawLine(pat, chars)
   chars = chars or 60
   pat = reify(pat)
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
                  -- TODO: proper dump
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
mt.drawLine = drawLine

M.id = id
M.T = T
M.maxi = maxi
M.pipe = ut.pipe
M.dump = ut.dump
M.t = TYPES
M.mt = mt

return M
