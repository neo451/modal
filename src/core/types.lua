local T = require("modal.utils").T
local decimaltofraction, gcd, lcm
local ut = require "modal.utils"
local compare, dump = ut.compare, ut.dump
local Time, Span, Event, State
local abs = math.abs
local floor = math.floor

local mt = { __class = "fraction" }
local span_mt = { __class = "span" }
local state_mt = { __class = "state" }
local event_mt = { __class = "event" }
span_mt.__index = span_mt
event_mt.__index = event_mt
state_mt.__index = state_mt
mt.__index = mt

function span_mt:spanCycles()
   local spans = {}
   local b, e = self._begin, self._end
   local e_sam = e:sam()
   -- TODO: zero width???
   -- if b == e then
   --    return { Span(b, e) }
   -- end
   while e > b do
      if b:sam() == e_sam then
         spans[#spans + 1] = Span(b, self._end)
         break
      end
      local next_b = b:nextSam()
      spans[#spans + 1] = Span(b, next_b)
      b = next_b
   end
   return spans
end

function span_mt:duration()
   return self._end - self._begin
end

function span_mt:midpoint()
   return self._begin + (self:duration() / 2)
end

function span_mt:cycleSpan()
   local b = self._begin:cyclePos()
   return Span(b, b + self:duration())
end

function span_mt:__eq(rhs)
   return self._begin == rhs._begin and self._end == rhs._end
end

function span_mt:__tostring()
   return self._begin:show() .. " → " .. self._end:show()
end

function span_mt:show()
   return self:__tostring()
end

function span_mt:withTime(func)
   return Span(func(self._begin), func(self._end))
end

function span_mt:withEnd(func)
   return Span(self._begin, func(self._end))
end

function span_mt:withCycle(func)
   local sam = self._begin:sam()
   local b = sam + func(self._begin - sam)
   local e = sam + func(self._end - sam)
   return Span(b, e)
end

function span_mt:sect(other)
   local maxOfStart = self._begin:max(other._begin)
   local minOfEnd = self._end:min(other._end)
   if maxOfStart > minOfEnd then
      return nil
   end
   if maxOfStart == minOfEnd then
      if maxOfStart == self._end and self._begin < self._end then
         return nil
      end
      if maxOfStart == other._end and other._begin < other._end then
         return nil
      end
   end
   return Span(maxOfStart, minOfEnd)
end

function span_mt:sect_e(other)
   local result = self:sect(other)
   if not result then
      error "Span: spans do not intersect"
   end
   return result
end

function Span(b, e)
   local new_obj = setmetatable({}, span_mt)
   b = b or 1
   e = e or 1
   new_obj._begin, new_obj._end = Time(b), Time(e)
   return new_obj
end

function event_mt:__eq(other)
   --    return (self.part == other.part)
   --       and (self.whole == other.whole)
   --       and (compare(self.value, other.value))
   --       and (compare(self.context, other.context))
   --       and (self.stateful == other.stateful)
   return self:__tostring() == other:__tostring()
end

function event_mt:duration()
   return self.whole._end - self.whole._begin
end

function event_mt:wholeOrPart()
   if self.whole ~= nil then
      return self.whole
   end
   return self.part
end

function event_mt:hasWhole()
   return self.whole ~= nil
end

function event_mt:hasOnset()
   return self.whole ~= nil and self.whole._begin == self.part._begin
end

function event_mt:withSpan(func)
   local whole = self.whole
   if whole ~= nil then
      whole = func(whole)
   end
   return Event(whole, func(self.part), self.value, self.context, self.stateful)
end

function event_mt:withValue(func)
   return Event(self.whole, self.part, func(self.value), self.context, self.stateful)
end

function event_mt:show()
   return self:__tostring()
end

function event_mt:__tostring()
   local part = self.part:__tostring()
   local h, t = "", ""
   if self:hasWhole() then
      h = (self.whole._begin ~= self.part._begin) and self.whole._begin:show() .. "-" or ""
      t = (self.whole._end ~= self.part._end) and "-" .. self.whole._end:show() or ""
   end
   return string.format("%s(%s)%s | %s", h, part, t, dump(self.value))
end

function event_mt:spanEquals(other)
   return ((other.whole == nil) and (self.whole == nil)) or (other.whole == self.whole)
end

function event_mt:setContext(newContext)
   return Event(self.whole, self.part, self.value, newContext, self.stateful)
end

function event_mt:combineContext(other)
   local newContext = {}
   for key, value in pairs(self.context) do
      newContext[key] = value
   end
   for key, value in pairs(other.context) do
      newContext[key] = value
   end
   local loc1 = self.context.locations or {}
   local loc2 = other.context.locations or {}
   local newloc = {}
   for _, value in ipairs(loc1) do
      table.insert(newloc, value)
   end
   for _, value in ipairs(loc2) do
      table.insert(newloc, value)
   end
   newContext.locations = newloc
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
   }, event_mt)
end

function state_mt:setSpan(span)
   return State(span, self.controls)
end

function state_mt:withSpan(func)
   return self:setSpan(func(self.span))
end

function state_mt:setControls(controls)
   return State(self.span, controls)
end

function state_mt:__tostring()
   return "span: " .. self.span:show() .. " controls: " .. dump(self.controls)
end

function state_mt:show()
   return self.__tostring(self)
end

function state_mt:__eq(other)
   return self.span == other.span and compare(self.controls, other.controls)
end

function State(span, controls)
   span = span or Span()
   controls = controls or {}
   return setmetatable({
      span = span,
      controls = controls,
   }, state_mt)
end

decimaltofraction = function(x0, err)
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

gcd = function(a, b)
   return (b == 0) and a or gcd(b, a % b)
end

lcm = function(a, b)
   return (a == 0 or b == 0) and 0 or abs(a * b) / gcd(a, b)
end

function mt:wholeCycle()
   return Span(self:sam(), self:nextSam())
end

function mt:cyclePos()
   return self - self:sam()
end

function mt:__add(f2)
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

function mt:__sub(f2)
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

function mt:__div(f2)
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

function mt:__mul(f2)
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

function mt:__pow(f2)
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

function mt:__mod(f2)
   f2 = Time(f2)
   local da = self.denominator
   local db = f2.denominator
   local na = self.numerator
   local nb = f2.numerator
   return Time((na * db) % (nb * da), da * db)
end

function mt:__unm()
   return Time(-self.numerator, self.denominator, false)
end

function mt:__eq(rhs)
   return self.numerator / self.denominator == rhs.numerator / rhs.denominator
end

function mt:__lt(rhs)
   return self.numerator / self.denominator < rhs.numerator / rhs.denominator
end

function mt:__lte(rhs)
   return self.numerator / self.denominator <= rhs.numerator / rhs.denominator
end

function mt:eq(rhs)
   return self == (Time(rhs))
end

function mt:lt(rhs)
   return self < Time(rhs)
end

function mt:reverse()
   return Time(1) / self
end

function mt:floor()
   return floor(self.numerator / self.denominator)
end

function mt:sam()
   return Time(self:floor())
end

function mt:nextSam()
   return self:sam() + 1
end

function mt:min(other)
   other = Time(other)
   if self < other then
      return self
   else
      return other
   end
end

function mt:max(other)
   other = Time(other)
   if self > other then
      return self
   else
      return other
   end
end

function mt:gcd(other)
   other = Time(other)
   local gcd_numerator = gcd(self.numerator, other.numerator)
   local lcm_denominator = lcm(self.denominator, other.denominator)
   return Time(gcd_numerator, lcm_denominator)
end

function mt:asFloat()
   return self.numerator / self.denominator
end

function mt:__tostring()
   return string.format("%d/%d", self.numerator, self.denominator)
end

function mt:show()
   return self:__tostring()
end

---@class Fraction
function Time(n, d, normalize)
   -- HACK:
   if T(n) == "fraction" then
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
   }, mt)
end

return { Span = Span, Event = Event, State = State, Time = Time }
