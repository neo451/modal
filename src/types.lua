local ut = require "ut"
local losc = require "losc" -- TODO: get rid of ??? core should be pure
local types = {}

local T = ut.T
local union = ut.union
local abs = math.abs
local floor = math.floor
local setmetatable = setmetatable
local tremove = table.remove
local tconcat = table.concat
local unpack = _G.unpack or table.unpack
local is_array = ut.is_array

local Time, Span, Event
local time = { __class = "time" }
local span = { __class = "span" }
local event = { __class = "event" }
span.__index = span
event.__index = event
time.__index = time

function span:spanCycles()
   local spans = {}
   local b, e = self.start, self.stop
   local e_sam = e:sam()
   -- TODO: zero width???
   -- if b == e then
   --    return { Span(b, e) }
   -- end
   while e > b do
      if b:sam() == e_sam then
         spans[#spans + 1] = Span(b, self.stop)
         break
      end
      local next_b = b:nextSam()
      spans[#spans + 1] = Span(b, next_b)
      b = next_b
   end
   return spans
end

function span:duration()
   return self.stop - self.start
end

function span:midpoint()
   return self.start + (self:duration() / 2)
end

function span:cycleSpan()
   local b = self.start:cyclePos()
   return Span(b, b + self:duration())
end

function span:__eq(rhs)
   return self.start == rhs.start and self.stop == rhs.stop
end

function span:__tostring()
   return self.start:show() .. " → " .. self.stop:show()
end

function span:show()
   return self:__tostring()
end

function span:withTime(func)
   return Span(func(self.start), func(self.stop))
end

function span:withEnd(func)
   return Span(self.start, func(self.stop))
end

function span:withCycle(func)
   local sam = self.start:sam()
   local b = sam + func(self.start - sam)
   local e = sam + func(self.stop - sam)
   return Span(b, e)
end

function span:sect(other)
   local maxOfStart = self.start:max(other.start)
   local minOfEnd = self.stop:min(other.stop)
   if maxOfStart > minOfEnd then
      return nil
   end
   if maxOfStart == minOfEnd then
      if maxOfStart == self.stop and self.start < self.stop then
         return nil
      end
      if maxOfStart == other.stop and other.start < other.stop then
         return nil
      end
   end
   return Span(maxOfStart, minOfEnd)
end

function span:sect_e(other)
   local result = self:sect(other)
   if not result then
      error "Span: spans do not intersect"
   end
   return result
end

function Span(b, e)
   b = b or 1
   e = e or 1
   return setmetatable({
      start = Time(b),
      stop = Time(e),
   }, span)
end

function event:__eq(other)
   --    return (self.part == other.part)
   --       and (self.whole == other.whole)
   --       and (compare(self.value, other.value))
   --       and (compare(self.context, other.context))
   --       and (self.stateful == other.stateful)
   return self:__tostring() == other:__tostring()
end

function event:duration()
   return self.whole.stop - self.whole.start
end

function event:wholeOrPart()
   if self.whole ~= nil then
      return self.whole
   end
   return self.part
end

function event:hasWhole()
   return self.whole ~= nil
end

function event:hasOnset()
   return self.whole ~= nil and self.whole.start == self.part.start
end

function event:withSpan(func)
   local whole = self.whole
   if whole ~= nil then
      whole = func(whole)
   end
   return Event(whole, func(self.part), self.value, self.context, self.stateful)
end

function event:withValue(func)
   return Event(self.whole, self.part, func(self.value), self.context, self.stateful)
end

function event:show()
   return self:__tostring()
end

function event:__tostring()
   local part = self.part:__tostring()
   local h, t = "", ""
   if self:hasWhole() then
      h = (self.whole.start ~= self.part.start) and self.whole.start:show() .. "-" or ""
      t = (self.whole.stop ~= self.part.stop) and "-" .. self.whole.stop:show() or ""
   end
   return ("%s(%s)%s | %s"):format(h, part, t, ut.tdump(self.value))
end

function event:spanEquals(other)
   return ((other.whole == nil) and (self.whole == nil)) or (other.whole == self.whole)
end

function event:setContext(newContext)
   return Event(self.whole, self.part, self.value, newContext, self.stateful)
end

function event:combineContext(other)
   local newContext = {}
   for key, value in pairs(self.context) do
      newContext[key] = value
   end
   for key, value in pairs(other.context) do
      newContext[key] = value
   end
   local loc1 = self.context.locations or {}
   local loc2 = other.context.locations or {}
   for i = 1, #loc2 do
      loc1[#loc1 + 1] = loc2[i]
   end
   newContext.locations = loc1
   return newContext
end

function Event(whole, part, value, context, stateful)
   part = part or Span()
   context = context or {}
   stateful = stateful or false
   if stateful and T(value) ~= "function" then
      error "Event: stateful event values must be of type function"
   end
   return setmetatable({
      whole = whole,
      part = part,
      value = value,
      context = context,
      stateful = stateful,
   }, event)
end

local function decimaltofraction(x0, err)
   err = err or 0.0000000001
   local num, den
   local g = abs(x0)
   local sign = x0 / g
   local a, b, c, d = 0, 1, 1, 0
   local s
   local iter = 0
   while iter < 1000000 do
      s = floor(g)
      num = a + s * c
      den = b + s * d
      a, b, c, d = c, d, num, den
      g = 1.0 / (g - s)
      iter = iter + 1
      if err > abs(sign * num / den - x0) then
         return sign * num, den
      end
   end
   error("Time: failed to find a fraction for " .. x0)
   return 0, 1
end

local function gcd(a, b)
   return (b == 0) and a or gcd(b, a % b)
end

local function lcm(a, b)
   return (a == 0 or b == 0) and 0 or abs(a * b) / gcd(a, b)
end

function time:wholeCycle()
   return Span(self:sam(), self:nextSam())
end

function time:cyclePos()
   return self - self:sam()
end

function time:__add(f2)
   f2 = Time(f2)
   local na = self.numerator
   local nb = f2.numerator
   local da = self.denominator
   local db = f2.denominator
   local g = gcd(da, db)
   if g == 1 then
      Time(na * db + da * nb, da * db, false)
   end
   local s = floor(da / g)
   local t = na * floor(db / g) + nb * s
   local g2 = gcd(t, g)
   if g2 == 1 then
      Time(t, s * db, false)
   end
   return Time(floor(t / g2), s * floor(db / g2), false)
end

function time:__sub(f2)
   f2 = Time(f2)
   local na = self.numerator
   local nb = f2.numerator
   local da = self.denominator
   local db = f2.denominator
   local g = gcd(da, db)
   if g == 1 then
      Time(na * db - da * nb, da * db, false)
   end
   local s = floor(da / g)
   local t = na * floor(db / g) - nb * s
   local g2 = gcd(t, g)
   if g2 == 1 then
      Time(t, s * db, false)
   end
   return Time(floor(t / g2), s * floor(db / g2), false)
end

function time:__div(f2)
   f2 = Time(f2)
   local na = self.numerator
   local nb = f2.numerator
   local da = self.denominator
   local db = f2.denominator
   local g1 = gcd(na, nb)
   if g1 > 1 then
      na = floor(na / g1)
      nb = floor(nb / g1)
   end
   local g2 = gcd(db, da)
   if g2 > 1 then
      da = floor(da / g2)
      db = floor(db / g2)
   end
   local n = na * db
   local d = nb * da
   if d < 0 then
      n = -n
      d = -d
   end
   return Time(n, d, false)
end

function time:__mul(f2)
   f2 = Time(f2)
   local na = self.numerator
   local nb = f2.numerator
   local da = self.denominator
   local db = f2.denominator
   local g1 = gcd(na, db)
   if g1 > 1 then
      na = floor(na / g1)
      db = floor(db / g1)
   end
   local g2 = gcd(nb, da)
   if g2 > 1 then
      nb = floor(nb / g2)
      da = floor(da / g2)
   end
   return Time(na * nb, da * db, false)
end

function time:__pow(f2)
   f2 = Time(f2)
   if f2.denominator == 1 then
      local power = f2.numerator
      if power >= 0 then
         return Time(self.numerator ^ power, self.denominator ^ power, false)
      elseif self.numerator >= 0 then
         return Time(self.denominator ^ -power, self.numerator ^ -power, false)
      else
         return Time((-self.numerator) ^ -power, (-self.denominator) ^ -power, false)
      end
   else
      return (self.numerator / self.denominator) ^ (f2.numerator / f2.denominator)
   end
end

function time:__mod(f2)
   f2 = Time(f2)
   local da = self.denominator
   local db = f2.denominator
   local na = self.numerator
   local nb = f2.numerator
   return Time((na * db) % (nb * da), da * db)
end

function time:__unm()
   return Time(-self.numerator, self.denominator, false)
end

function time:__eq(rhs)
   return self.numerator / self.denominator == rhs.numerator / rhs.denominator
end

function time:__lt(rhs)
   return self.numerator / self.denominator < rhs.numerator / rhs.denominator
end

function time:__lte(rhs)
   return self.numerator / self.denominator <= rhs.numerator / rhs.denominator
end

function time:eq(rhs)
   return self == (Time(rhs))
end

function time:lt(rhs)
   return self < Time(rhs)
end

function time:gt(rhs)
   return self > Time(rhs)
end

function time:lte(rhs)
   return self <= Time(rhs)
end

function time:gte(rhs)
   return self <= Time(rhs)
end

function time:reverse()
   return Time(1) / self
end

function time:floor()
   return floor(self.numerator / self.denominator)
end

function time:sam()
   return Time(self:floor())
end

function time:nextSam()
   return self:sam() + 1
end

function time:min(other)
   other = Time(other)
   if self < other then
      return self
   else
      return other
   end
end

function time:max(other)
   other = Time(other)
   if self > other then
      return self
   else
      return other
   end
end

function time:gcd(other)
   other = Time(other)
   local gcd_numerator = gcd(self.numerator, other.numerator)
   local lcm_denominator = lcm(self.denominator, other.denominator)
   return Time(gcd_numerator, lcm_denominator)
end

function time:asFloat()
   return self.numerator / self.denominator
end

function time:__tostring()
   return ("%d/%d"):format(self.numerator, self.denominator)
end

function time:show()
   return self:__tostring()
end

---@class Fraction
function Time(n, d, normalize)
   -- HACK:
   if T(n) == "time" then
      return n
   end
   n = n or 0
   d = d or 1
   if normalize == nil then
      normalize = true
   end
   if n % 1 ~= 0 then
      n, d = decimaltofraction(n)
   end
   if d == 0 then
      error "Time: divide by zero"
   end
   if normalize and (n ~= 0) then
      local g = floor(gcd(n, d))
      n = floor(n / g)
      d = floor(d / g)
   end
   return setmetatable({
      numerator = n,
      denominator = d,
   }, time)
end

local stream = { __class = "stream" }

function stream:notifyTick(cycleFrom, cycleTo, s, cps, bpc, mill, now, State)
   if not self.pattern then
      return
   end
   local events = self.pattern:onsetsOnly()(cycleFrom, cycleTo, State)
   for _, ev in ipairs(events) do
      local cycleOn = ev.whole.start
      local cycleOff = ev.whole.stop
      local linkOn = s:time_at_beat(cycleOn:asFloat() * bpc, 0)
      local linkOff = s:time_at_beat(cycleOff:asFloat() * bpc, 0)
      local deltaSeconds = (linkOff - linkOn) / mill
      local value = ev.value
      value.cps = ev.value.cps or cps
      value.cycle = cycleOn:asFloat()
      value.delta = deltaSeconds
      local link_secs = now / mill
      local nudge = 0
      local diff = losc:now() + -link_secs
      -- print(link_secs)
      -- print(diff:seconds())
      local ts = diff + (linkOn / mill) + self.latency + nudge
      self.callback(value, ts)
   end
end
stream.__index = stream

local function Stream(callback)
   return setmetatable({ latency = 0.2, callback = callback }, stream)
end

local lpeg = require "lpeg"
local P, S, V, R, C, Ct = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.C, lpeg.Ct

local function pId(...)
   return { tconcat { ... } }
end

local function pComp(const, tvar)
   return { constructor = const[1], tvar[1] }
end

local function pDef(...)
   local args = { ... }
   local name
   if args[1].isname then
      name = tremove(args, 1)[1]
   end
   local ret = tremove(args, #args)
   return { ret = ret, name = name, unpack(args) }
end

local function pTab(a)
   a.istable = true
   return a
end

local typedef = V "typedef"
local fdef = V "fdef"
local tab = V "tab"
local elem = V "elem"
local comp_type = V "comp_type"
local char = R("AZ", "az")
local name = V "name"
local ws = S " \n\r\t" ^ 0
local id = ws * ((char ^ 1) / pId) * ws

local rules = {
   [1] = "typedef",
   name = id * ws * P "::" * ws / function(a)
      a.isname = true
      return a
   end,
   typedef = name ^ -1 * (elem * ws * P "->" * ws) ^ 1 * elem / pDef,
   elem = comp_type + id + fdef + tab,
   fdef = P "(" * ws * typedef * ws * P ")",
   tab = P "[" * ws * elem * ws * P "]" / pTab,
   comp_type = id * ws * id / pComp,
}

local grammar = Ct(C(rules))

local function TDef(a)
   local tdef = grammar:match(a)[2]
   tdef.source = a
   setmetatable(tdef, {
      __tostring = function(self)
         return self.source
      end,
   })
   return tdef, tdef.name
end

local valuemap = {
   -- TODO: cover if other types
   __add = function(t1, t2)
      if type(t2) == "number" then
         local k, v = next(t1)
         return { [k] = v + t2 }
      elseif type(t2) == "table" and not is_array(t2) then
         for k, v in pairs(t1) do
            if type(v) == "number" or tonumber(v) then
               t1[k] = v + (t2[k] or 0)
            end
         end
         for k, v in pairs(t2) do
            if not t1[k] then
               t1[k] = v
            end
         end
         return t1
      else
         error "bad table arith"
      end
   end,
   __sub = function(t1, t2)
      if type(t2) == "number" then
         local k, v = next(t1)
         return { [k] = v - t2 }
      elseif type(t2) == "table" and not is_array(t2) then
         for k, v in pairs(t1) do
            if type(v) == "number" or tonumber(v) then
               t1[k] = v - (t2[k] or 0)
            end
         end
         for k, v in pairs(t2) do
            if not t1[k] then
               t1[k] = v
            end
         end
         return t1
      else
         error "bad table arith"
      end
   end,
   __unm = function(t)
      local k, v = next(t)
      -- TODO: check
      return { [k] = -v }
   end,
   __concat = function(lhs, rhs)
      return union(lhs, rhs)
   end,
}
valuemap.__index = valuemap

local function ValueMap(valmap)
   return setmetatable(valmap, valuemap)
end

types = { Span = Span, Event = Event, Time = Time, Stream = Stream, TDef = TDef, ValueMap = ValueMap }

return types
