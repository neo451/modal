require "modal"()

-- local wrap = coroutine.wrap
local wrap = coroutine.create

local yield = coroutine.yield

local function Pattern(query)
   query = query or function()
      return {}
   end
   return setmetatable({ query = wrap(query) }, mt)
end

function pure(value)
   local query = function(state)
      local cycles = state.span:spanCycles()
      local f = function(span)
         local whole = span._begin:wholeCycle()
         return Event(whole, span, value)
      end
      for _, v in pairs(cycles) do
         yield(f(v))
      end
      return cycles
   end
   return Pattern(query)
end

local function querySpan(pat, b, e)
   local state = State(Span(b, e))
   return function()
      local code, res = coroutine.resume(pat.query, state)
      if code then
         return res
      else
         return nil
      end
   end
end
mt.querySpan = querySpan

function mt:__call(b, e)
   return querySpan(self, b, e)
end

for v in pure "1"(0, 1) do
   print(v)
end
