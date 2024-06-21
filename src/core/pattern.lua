local ut = require "modal.utils"
local id = ut.id
local map = ut.map
local filter = ut.filter
local dump = ut.dump
local T = ut.T

local bjork = require "modal.euclid"
local getScale = require "modal.scales"
local types = require "modal.types"
local Event, Span, State, Time = types.Event, types.Span, types.State, types.Time
local TDef = require "modal.typedef"
local maxi = require "modal.maxi"
local fun = require "modal.fun"

local unpack = unpack or table.unpack
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

local M = {}
local U = {} -- Unpatternified versions
local TYPES = {}

M.mini = maxi(M, false)
M.sl = ut.string_lambda(M)

local function reify(thing)
   local t = T(thing)
   if "string" == t then
      -- TODO: try to read as a string_lambda before pattern, make two structure diff
      -- local ok, f = pcall(M.sl, thing)
      -- if ok then
      --    return f
      -- end
      local res = M.mini("[" .. thing .. "]")
      return res and res() or M.silence()
   elseif "table" == t then
      return M.fastcat(thing)
   elseif "pattern" == t then
      return thing
   else
      return M.pure(thing)
   end
end
M.reify = reify

local mt = { __class = "pattern" }

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
   return M.op["|>"](self, other)
end

function mt:__add(other)
   return M.op["|+"](self, other)
end

function mt:__sub(other)
   return M.op["|-"](self, other)
end

function mt:__mul(other)
   return M.op["|*"](self, other)
end

function mt:__div(other)
   return M.op["|/"](self, other)
end

function mt:__mod(other)
   return M.op["|%"](self, other)
end

function mt:__pow(other)
   return M.op["|^"](self, other)
end

function mt:slowcat(pats)
   pats[#pats + 1] = self
   return M.slowcat(pats)
end

function mt:fastcat(pats)
   pats[#pats + 1] = self
   return M.fastcat(pats)
end

function mt:stack(pats)
   pats[#pats + 1] = self
   return M.stack(pats)
end

mt.__index = mt

-- automatically export pattern methods
setmetatable(M, {
   __index = function(_, k)
      return mt[k]
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
   return setmetatable(pat:query(state), {
      __tostring = function(self)
         return ut.dump(self)
      end,
   })
end
mt.querySpan = querySpan

local function filterEvents(pat, func)
   local query = function(_, state)
      local events = pat:query(state)
      return filter(func, events)
   end
   return Pattern(query)
end
mt.filterEvents = filterEvents

local function filterValues(pat, condf)
   local query = function(_, state)
      local events = pat:query(state)
      local f
      f = function(event)
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
   local query = function(_, state)
      local cycles = state.span:spanCycles()
      local f = function(subspan)
         return pat:query(state:setSpan(subspan))
      end
      -- TODO: replace
      return ut.flatten(map(f, cycles))
   end
   return Pattern(query)
end
mt.splitQueries = splitQueries

local function withValue(pat, func)
   local query = function(_, state)
      local events = pat:query(state)
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
   local query = function(_, state)
      local new_state = state:withSpan(func)
      return pat:query(new_state)
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
   return Pattern(function(_, state)
      return func(pat:query(state))
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
   local query = function(_, state)
      local events = pat:query(state)
      return map(function(ev)
         return ev:withSpan(func)
      end, events)
   end
   return Pattern(query)
end
mt.withEventSpan = withEventSpan

local function withEventTime(pat, func)
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
mt.appWhole = appWhole

-- Tidal's <*>
local function appBoth(pat, pat_val)
   local whole_func = function(span_a, span_b)
      if not span_a or not span_b then
         return
      end
      return span_a:sect(span_b)
   end
   return M.appWhole(pat, whole_func, pat_val)
end
mt.appBoth = appBoth

-- Tidal's <*
local function appLeft(pat, pat_val)
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
mt.appLeft = appLeft

-- Tidal's *>
local function appRight(pat, pat_val)
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
mt.appRight = appRight

local function bindWhole(pat, choose_whole, func)
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
      return ut.flatten(map(match, events))
   end
   return Pattern(query)
end
mt.bindWhole = bindWhole

local function bind(pat, func)
   local whole_func
   whole_func = function(a, b)
      if a == nil or b == nil then
         return nil
      end
      return a:sect(b)
   end
   return M.bindWhole(pat, whole_func, func)
end
mt.bind = bind

local function join(pat)
   return M.bind(pat, id)
end
mt.join = join

local function outerBind(pat, func)
   return M.bindWhole(pat, function(a)
      return a
   end, func)
end
mt.outerBind = outerBind

local function innerBind(pat, func)
   return M.bindWhole(pat, function(_, b)
      return b
   end, func)
end
mt.innerBind = innerBind

local function outerJoin(pat)
   return M.outerBind(pat, id)
end
mt.outerJoin = outerJoin

local function innerJoin(pat)
   return M.innerBind(pat, id)
end
mt.innerJoin = innerJoin

local function squeezeJoin(pat)
   local query
   query = function(_, state)
      local events = M.discreteOnly(pat):query(state)
      local flatEvent
      flatEvent = function(outerEvent)
         local span = outerEvent:wholeOrPart()
         local innerPat = M.focus(span._begin, span._end, outerEvent.value)
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
      local result = ut.flatten(map(flatEvent, events))
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
      a, b = fmap(reify(a), ut.curry(f, 2)), reify(b)
      return appLeft(a, b):removeNils()
   end
end

function _op.Out(f)
   return function(a, b)
      a, b = fmap(reify(a), ut.curry(f, 2)), reify(b)
      return appRight(a, b):removeNils()
   end
end

function _op.Mix(f)
   return function(a, b)
      a, b = fmap(reify(a), ut.curry(f, 2)), reify(b)
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
   uni = function (a, b) return ut.union(a, b) end, -- TODO: use local
   funi = function (a, b) return ut.flip(ut.union)(a, b) end,
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
         local symb = str_format(how_format[v], op_set[k])
         op[symb] = _op[v](f)
      end
   end
end
op["#"] = op["|>"]

M.op = op

local function silence()
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
         local whole = span._begin:wholeCycle()
         return Event(whole, span, value)
      end
      return map(f, cycles)
   end
   return Pattern(query)
end
M.pure = pure

function M.purify(value)
   if T(value) == "pattern" then
      return value
   else
      return M.pure(value)
   end
end

local patternify = function(func)
   local patterned = function(...)
      local arity = ut.nparams(func)
      -- TODO:
      -- local pats = map(reify, { ... })
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
      mapFn = ut.curry(mapFn, arity - 1)
      return reduce(appLeft, fmap(left, mapFn), pats):innerJoin()
   end
   return patterned
end

local function typecheck(f, name)
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
                     v[j] = M.purify(vv)
                  end
               end
            end
         else
            if T(tvar) == "table" or tvar == "f" then
               v = M.sl(v)
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

local function register(args)
   local name, type_sig, f, nify = unpack(args)
   if T(nify) == "nil" then
      nify = true
   end
   local arity = ut.nparams(f)
   if nify then
      TYPES[name] = TDef(type_sig)
      U[name] = f
      local f_p = patternify(f)
      local f_p_t = typecheck(f_p, name)
      local f_c_p_t = ut.auto_curry(arity, f_p_t)
      M[name] = f_c_p_t
      rawset(mt, name, ut.method_wrap(f_c_p_t))
      -- mt[name] = ut.method_wrap(f_c_p_t)
   else
      TYPES[name] = TDef(type_sig)
      local f_t = typecheck(f, name)
      local f_t_c = ut.auto_curry(arity, f_t)
      M[name] = f_t_c
      rawset(mt, name, ut.method_wrap(f_t))
      -- mt[name] = ut.method_wrap(f_t)
   end
end
M.register = register

local function register2(args)
   local type_sig, f, nify = unpack(args)
   local tdef, name = TDef(type_sig)
   if T(nify) == "nil" then
      nify = true
   end
   local arity = ut.nparams(f)
   if nify then
      TYPES[name] = tdef
      U[name] = f
      local f_p = patternify(f)
      local f_p_t = typecheck(f_p, name)
      local f_c_p_t = ut.auto_curry(arity, f_p_t)
      M[name] = f_c_p_t
      rawset(mt, name, ut.method_wrap(f_c_p_t))
   else
      TYPES[name] = TDef(type_sig)
      local f_t = typecheck(f, name)
      local f_t_c = ut.auto_curry(arity, f_t)
      M[name] = f_t_c
      rawset(mt, name, ut.method_wrap(f_t))
   end
end
M.register = register

---stack two patterns
---@param a any
---@param b any
---@return Pattern
local function overlay(a, b)
   local query = function(_, st)
      return ut.concat(a:query(st), b:query(st))
   end
   return Pattern(query)
end
register2 { "overlay :: Pattern a -> Pattern a -> Pattern a", overlay, false }

---stack a table of patterns
---@param pats table<any>
---@return Pattern
local function stack(pats)
   return reduce(M.overlay, silence(), iter(pats))
end
register2 { "stack :: [Pattern a] -> Pattern a", stack, false }

-- register {
--    "polymeter",
-- "Pattern Int -> [Pattern a] -> Pattern a",
-- TODO: need to patternify steps
function M.polymeter(steps, pats)
   -- local nsteps, ratio, res = reify(steps), nil, {}
   local res, ratio = {}, nil
   for _, pat in iter(pats) do
      ratio = steps / #((pat)(0, 1))
      res[#res + 1] = U.fast(ratio, pat)
   end
   return stack(res)
end
--    false,
-- }

---cat pattern per cycle
---@param pats any
---@return Pattern
local function slowcat(pats)
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
register2 { "slowcat :: [Pattern a] -> Pattern a", slowcat, false }

local function fastcat(pats)
   return U.fast(#pats, M.slowcat(pats))
end
register2 { "fastcat :: [Pattern a] -> Pattern a", fastcat, false }

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
   return M.slow(total, M.timecat(args))
end

local function superimpose(f, pat)
   return stack { pat, f(pat) }
end

register2 { "superimpose :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a", superimpose, false }

local function layer(tf, pat)
   local acc = {}
   for i, f in iter(tf) do
      acc[i] = f(pat)
   end
   return stack(acc)
end
register2 { "layer :: [(Pattern a -> Pattern b)] -> Pattern a -> Pattern b", layer } -- a little ugly lol layer, false,

local function fast(factor, pat)
   factor = Time(factor) -- TODO:
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
end
register2 { "fast :: Pattern Time -> Pattern a -> Pattern a", fast }

local function slow(factor, pat)
   -- factor = Time(factor)
   if factor:eq(0) then
      return silence()
   else
      return U.fast(factor:reverse(), pat)
   end
end

register2 { "slow :: Pattern Time -> Pattern a -> Pattern a", slow }

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
   false,
} -- HACK: why not patternify TIME??

-- rotR
register {
   "late",
   "Time -> Pattern a -> Pattern a",
   function(offset, pat)
      return M.early(-offset, pat)
   end,
   false,
}

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
         return U.fast(n, M.pure(x))
      end)
      return squeezeJoin(pat)
   end,
}

register {
   "fastgap",
   "Pattern Time -> Pattern a -> Pattern a",
   function(factor, pat)
      factor = Time(factor)
      if factor <= Time(0) then
         return silence()
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
      if b > e or e > Time(1) or b > Time(1) or b < Time(0) or e < Time(0) then
         return silence()
      end
      local fasted = U.fastgap((e - b):reverse(), pat)
      return M.late(b, fasted)
   end,
   false,
}

register {
   "focus",
   "Time -> Time -> Pattern a -> Pattern a",
   function(b, e, pat)
      local fasted = U.fast((e - b):reverse(), pat)
      return M.late(b:cyclePos(), fasted)
   end,
   false,
}

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
      return withEventSpan(withQuerySpan(pat, qf), ef):splitQueries()
   end,
   false,
}

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

register2 { "run :: Pattern Int -> Pattern Int", run, false }

local _scan = function(n)
   local res = {}
   for _, v in fun.range(1, n) do
      res[#res + 1] = M.run(v)
   end
   return slowcat(res)
end

local function scan(n)
   return fmap(n, _scan):join()
end
register2 { "scan :: Pattern Int -> Pattern Int", scan, false }

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
M.tri = fastcat { M.isaw, M.saw }
M.tri2 = fastcat { M.isaw2, M.saw2 }
M.time = waveform(id)
M.rand = waveform(ut.timeToRand)

M._irand = function(i)
   return fmap(M.rand, function(x)
      return floor(x * i)
   end)
end
-- TODO: register
M.irand = function(ipat)
   return fmap(reify(ipat), M._irand):innerJoin()
end

local _chooseWith = function(pat, ...)
   local vals = { ... }
   if #vals == 0 then
      return silence()
   end
   return fmap(M.range(1, #vals + 1, pat), function(i)
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
      return M.appLeft(
         fmap(pat, function(val)
            return function(_)
               return val
            end
         end),
         prand:filterValues(f)
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
      return fmap(by, function()
         return stack { U.degradeBy(by, pat), func(U.undegradeBy(1 - by, pat)) }
      end):innerJoin()
   end,
}

register {
   "sometimes",
   "(Pattern a -> Pattern a) -> Pattern a -> Pattern a",
   function(func, pat)
      return U.sometimesBy(0.5, func, pat)
   end,
}

--- Applies the given structure to the pattern, alias to op.keepif.Out
---@param boolpat table<boolean>
---@param pat any
---@return Pattern
local function struct(boolpat, pat)
   return op.keepif.Out(pat, boolpat)
end
register { "struct", "[Pattern bool] -> Pattern a -> Pattern a", struct, false }

local function euclid(n, k, pat)
   return struct(bjork(n, k, 0), pat)
end

local function euclidRot(n, k, rot, pat)
   return struct(bjork(n, k, rot), pat)
end

register2 { "euclid :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a", euclid }

register2 { "euclidRot :: Pattern Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a", euclidRot }

---reverse pattern in every cycle
---@param pat any
---@return Pattern
local function rev(pat)
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
end

register2 { "rev :: Pattern a -> Pattern a", rev }

local function iter_(n, pat)
   local acc = {}
   for i = 1, n do
      acc[i] = U.early((i - 1) / n, pat)
   end
   return slowcat(acc)
end
register2 { "iter :: Pattern Int -> Pattern a -> Pattern a", iter_ }

local function reviter(n, pat)
   local acc = {}
   for i = 1, n do
      acc[i] = M.late((i - 1) / n, pat)
   end
   return slowcat(acc)
end
register2 { "reviter :: Pattern Int -> Pattern a -> Pattern a", reviter }

local function segment(n, pat)
   return appLeft(M.fast(n, id), pat)
end
register2 { "segment :: Pattern Time -> Pattern a -> Pattern a", segment }

---limit value in pattern to a range TODO: see tidal impl
---@param mi number
---@param ma number
---@param pat number
---@return unknown
local function range(mi, ma, pat)
   return pat * (ma - mi) + mi
end
register2 { "range :: Pattern number -> Pattern number -> Pattern number -> Pattern a", range, false }

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
register {
   "when",
   "(Int -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a",
   function(test, f, pat)
      local query = function(_, state)
         local cycle_idx = state.span._begin:sam():asFloat()
         if test(cycle_idx) then
            return (f(pat)):query(state)
         else
            return pat:query(state)
         end
      end
      return Pattern(query):splitQueries()
   end,
   false,
}

local slowcatPrime = function(pats)
   local query = function(_, state)
      local len = #pats
      local index = state.span._begin:sam():asFloat() % len + 1
      local pat = pats[index]
      return pat:query(state)
   end
   return Pattern(query):splitQueries()
end

register {
   "every",
   -- HACK: "Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a",
   "Pattern Int -> Pattern f -> Pattern a -> Pattern a",
   function(n, f, pat)
      local acc = {}
      for i = 1, n do
         acc[i] = (i == 1) and f(pat) or pat
      end
      return slowcatPrime(acc)
   end,
}

register {
   "off",
   "Pattern Time -> Pattern f -> Pattern a -> Pattern a",
   function(tp, f, pat)
      return pat:overlay(f(pat:late(tp)))
   end,
}

register {
   "scale",
   -- TODO:
   -- "Pattern String -> Pattern a -> Pattern a",
   "String -> Pattern a -> Pattern a",
   function(name, pat)
      return fmap(pat, getScale(name))
   end,
   false,
}

---union two value maps .. bit weird ...
---@param pat ValueMap
---@param other ValueMap
---@return Pattern
local function concat(pat, other)
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
register2 { "concat :: Pattern a -> Pattern a -> Pattern a", concat, false }

M.id = id
M.T = T
M.maxi = maxi
M.pipe = ut.pipe
M.dump = ut.dump
M.u = U
M.t = TYPES
M.mt = mt

return M
