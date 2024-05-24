local Fraction, tofrac
do
   local _obj_0 = require("modal.fraction")
   Fraction, tofrac = _obj_0.Fraction, _obj_0.tofrac
end
local compare, dump
do
   local _obj_0 = require("modal.utils")
   compare, dump = _obj_0.compare, _obj_0.dump
end
local Span
do
   local _class_0
   local _base_0 = {
      type = function()
         return "span"
      end,
      spanCycles = function(self)
         local spans = {}
         if self._begin == self._end then
            local _ = {
               Span(self._begin, self._end),
            }
         end
         while self._end > self._begin do
            if self._begin:sam() == self._end:sam() then
               table.insert(spans, Span(self._begin, self._end))
               break
            end
            table.insert(spans, Span(self._begin, self._begin:nextSam()))
            self._begin = self._begin:nextSam()
         end
         return spans
      end,
      duration = function(self)
         return self._end - self._begin
      end,
      midpoint = function(self)
         return self._begin + (self:duration() / 2)
      end,
      cycleSpan = function(self)
         return Span(self:cyclePos(self._begin), self:cyclePos(self._begin) + self:duration())
      end,
      __eq = function(self, rhs)
         return self._begin == rhs._begin and self._end == rhs._end
      end,
      __tostring = function(self)
         return tostring(self._begin:show()) .. " → " .. tostring(self._end:show())
      end,
      show = function(self)
         return self:__tostring()
      end,
      withTime = function(self, func)
         return Span(func(self._begin), func(self._end))
      end,
      withEnd = function(self, func)
         return Span(self._begin, func(self._end))
      end,
      withCycle = function(self, func)
         local sam = self._begin:sam()
         local b = sam + func(self._begin - sam)
         local e = sam + func(self._end - sam)
         return Span(b, e)
      end,
      wholeCycle = function(self, frac)
         return Span(frac:sam(), frac:nextSam())
      end,
      cyclePos = function(self, frac)
         return frac - frac:sam()
      end,
      sect = function(self, other)
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
      end,
      sect_e = function(self, other)
         local result = self:sect(other)
         if not result then
            error("Span: spans do not intersect")
         end
         return result
      end,
   }
   _base_0.__index = _base_0
   _class_0 = setmetatable({
      __init = function(self, b, e)
         if b == nil then
            b = 1
         end
         if e == nil then
            e = 1
         end
         self._begin, self._end = tofrac(b), tofrac(e)
      end,
      __base = _base_0,
      __name = "Span",
   }, {
      __index = _base_0,
      __call = function(cls, ...)
         local _self_0 = setmetatable({}, _base_0)
         cls.__init(_self_0, ...)
         return _self_0
      end,
   })
   _base_0.__class = _class_0
   Span = _class_0
end
local Event
do
   local _class_0
   local _base_0 = {
      type = function()
         return "event"
      end,
      duration = function(self)
         return self.whole._end - self.whole._begin
      end,
      wholeOrPart = function(self)
         if self.whole ~= nil then
            return self.whole
         end
         return self.part
      end,
      hasWhole = function(self)
         return self.whole ~= nil
      end,
      hasOnset = function(self)
         return self.whole ~= nil and self.whole._begin == self.part._begin
      end,
      withSpan = function(self, func)
         local whole = self.whole
         if whole ~= nil then
            whole = func(whole)
         end
         return Event(whole, func(self.part), self.value, self.context, self.stateful)
      end,
      withValue = function(self, func)
         return Event(self.whole, self.part, func(self.value), self.context, self.stateful)
      end,
      show = function(self)
         return self:__tostring()
      end,
      __tostring = function(self)
         local partStartsWithWhole = self:hasWhole() and self.whole._begin == self.part._begin
         local partEndsWithWhole = self:hasWhole() and self.whole._end == self.part._end
         local partFormat = "(%s)"
         if partStartsWithWhole and partEndsWithWhole then
            partFormat = "%s"
         elseif partStartsWithWhole then
            partFormat = "(%s) ⇝"
         else
            partFormat = "(%s) ⇜"
         end
         local partString = string.format(partFormat, self.part:show())
         return "[" .. tostring(partString) .. " | " .. tostring(dump(self.value)) .. "]"
      end,
      spanEquals = function(self, other)
         return ((other.whole == nil) and (self.whole == nil)) or (other.whole == self.whole)
      end,
      __eq = function(self, other)
         return (self.part == other.part)
            and (self.whole == other.whole)
            and (compare(self.value, other.value))
            and (compare(self.context, other.context))
            and (self.stateful == other.stateful)
      end,
      setContext = function(self, newContext)
         return Event(self.whole, self.part, self.value, newContext, self.stateful)
      end,
      combineContext = function(self, other)
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
      end,
   }
   _base_0.__index = _base_0
   _class_0 = setmetatable({
      __init = function(self, whole, part, value, context, stateful)
         if whole == nil then
            whole = nil
         end
         if part == nil then
            part = Span()
         end
         if value == nil then
            value = nil
         end
         if context == nil then
            context = {}
         end
         if stateful == nil then
            stateful = false
         end
         if stateful and type(value) ~= "function" then
            error("Event: stateful event values must be of type function")
         end
         self.whole, self.part, self.value, self.context, self.stateful = whole, part, value, context, stateful
      end,
      __base = _base_0,
      __name = "Event",
   }, {
      __index = _base_0,
      __call = function(cls, ...)
         local _self_0 = setmetatable({}, _base_0)
         cls.__init(_self_0, ...)
         return _self_0
      end,
   })
   _base_0.__class = _class_0
   Event = _class_0
end
local State
do
   local _class_0
   local _base_0 = {
      type = function()
         return "state"
      end,
      setSpan = function(self, span)
         return State(span, self.controls)
      end,
      withSpan = function(self, func)
         return self:setSpan(func(self.span))
      end,
      setControls = function(self, controls)
         return State(self.span, controls)
      end,
      __tostring = function(self)
         return "span: " .. tostring(self.span:show()) .. " controls: " .. tostring(dump(self.controls))
      end,
      __eq = function(self, other)
         return self.span == other.span and compare(self.controls, other.controls)
      end,
   }
   _base_0.__index = _base_0
   _class_0 = setmetatable({
      __init = function(self, span, controls)
         if span == nil then
            span = Span()
         end
         if controls == nil then
            controls = {}
         end
         self.span = span
         self.controls = controls
      end,
      __base = _base_0,
      __name = "State",
   }, {
      __index = _base_0,
      __call = function(cls, ...)
         local _self_0 = setmetatable({}, _base_0)
         cls.__init(_self_0, ...)
         return _self_0
      end,
   })
   _base_0.__class = _class_0
   State = _class_0
end
return {
   Span = Span,
   Event = Event,
   State = State,
}
