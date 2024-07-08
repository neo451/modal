local ut = {}
local pattern = {}
local control = {}
local types = {}
local theory = {}
local notation = {}
local a2s = {}
local factory = {}
local has_lpeg, lpeg = pcall(require, "lpeg")
lpeg = has_lpeg and lpeg or require("lulpeg"):register(not _ENV and _G)
local has_socket, socket = pcall(require, "socket")
local has_al, al = pcall(require, "abletonlink")
local has_losc, losc = pcall(require, "losc")
local has_plugin, plugin = pcall(require, "losc.plugins.udp-socket")
_G.struct = nil
local has_RL, RL = pcall(require, "readline")
local Clock

do
   local pairs = pairs
   local ipairs = ipairs
   local tostring = tostring
   local loadstring = loadstring or load
   local setmetatable = setmetatable
   local type = type
   local unpack = unpack or rawget(table, "unpack")
   local str_dump = string.dump
   local str_char = string.char
   local tconcat = table.concat
   local tremove = table.remove
   local floor = math.floor
   local ceil = math.ceil
   local abs = math.abs
   local huge = math.huge
   local d_getinfo = debug.getinfo
   local d_getlocal = debug.getlocal
   local d_sethook = debug.sethook
   local d_gethook = debug.gethook
   local d_getupvalue = debug.getupvalue
   local d_setupvalue = debug.setupvalue
   
   Usecolor = false
   
   -- from https://www.lua.org/gems/sample.pdf
   -- TODO: smarter cache over time maybe
   local function memoize(f)
      local mem = {} -- memoizing table
      setmetatable(mem, { __mode = "kv" }) -- make it weak
      return function(x) -- new version of ’f’, with memoizing
         local r = mem[x]
         if r == nil then -- no previous result?
            r = f(x) -- calls original function
            mem[x] = r -- store result for reuse
         end
         return r
      end
   end
   ut.memoize = memoize
   ut.loadstring = memoize(loadstring)
   
   ---@table term colors
   local colors = {}
   
   local colormt = {}
   
   function colormt:__tostring()
      return self.value
   end
   
   function colormt:__concat(other)
      return tostring(self) .. tostring(other)
   end
   
   function colormt:__call(s)
      return self .. s .. colors.reset
   end
   
   local function makecolor(value)
      return setmetatable({ value = str_char(27) .. "[" .. tostring(value) .. "m" }, colormt)
   end
   
   local colorvalues = {
      -- attributes
      reset = 0,
      clear = 0,
      default = 0,
      bright = 1,
      dim = 2,
      underscore = 4,
      blink = 5,
      reverse = 7,
      hidden = 8,
   
      -- foreground
      black = 30,
      red = 31,
      green = 32,
      yellow = 33,
      blue = 34,
      magenta = 35,
      cyan = 36,
      white = 37,
   
      -- background
      onblack = 40,
      onred = 41,
      ongreen = 42,
      onyellow = 43,
      onblue = 44,
      onmagenta = 45,
      oncyan = 46,
      onwhite = 47,
   }
   
   for c, v in pairs(colorvalues) do
      colors[c] = makecolor(v)
   end
   
   ut.colors = colors
   
   ---@table bitwise ops
   local bit = {}
   local MOD = 2 ^ 32
   local MODM = MOD - 1
   
   -- TODO: replace with memoize ...?
   local bit_memo = function(f)
      local mt = {}
      local t = setmetatable({}, mt)
      mt.__index = function(self, k)
         local v = f(k)
         self.k = v
         return v
      end
      return t
   end
   
   local make_bitop_uncached = function(t, m)
      local bitop = function(a, b)
         local res, p = 0, 1
         while a ~= 0 and b ~= 0 do
            local am, bm = a % m, b % m
            res = res + t[am][bm] * p
            a = (a - am) / m
            b = (b - bm) / m
            p = p * m
         end
         res = res + (a + b) * p
         return res
      end
      return bitop
   end
   
   local make_bitop = function(t)
      local op1 = make_bitop_uncached(t, 2 ^ 1)
      local op2 = bit_memo(function(a)
         return bit_memo(function(b)
            return op1(a, b)
         end)
      end)
      return make_bitop_uncached(op2, 2 ^ (t.n or 1))
   end
   
   local tobit = function(x)
      return x % 2 ^ 32
   end
   bit.tobit = tobit
   
   local bxor = make_bitop {
      [0] = { [0] = 0, [1] = 1 },
      [1] = { [0] = 1, [1] = 0 },
      n = 4,
   }
   bit.bxor = bxor
   
   local bnot = function(a)
      return MODM - a
   end
   bit.bnot = bnot
   
   local band = function(a, b)
      return ((a + b) - bxor(a, b)) / 2
   end
   bit.band = band
   
   local bor = function(a, b)
      return MODM - band(MODM - a, MODM - b)
   end
   bit.bor = bor
   
   local lshift, rshift
   rshift = function(a, disp)
      if disp < 0 then
         return lshift(a, -disp)
      end
      return floor(a % 2 ^ 32 / 2 ^ disp)
   end
   bit.rshift = rshift
   
   lshift = function(a, disp)
      if disp < 0 then
         return rshift(a, -disp)
      end
      return (a * 2 ^ disp) % 2 ^ 32
   end
   bit.lshift = lshift
   
   ut.bit = bit
   
   ---Copyright (c) 2016 rxi
   ---@table log
   local log = { _version = "0.1.0" }
   ut.log = log
   
   log.usecolor = true
   log.outfile = nil
   log.level = "trace"
   
   local modes = {
      { name = "trace", color = "\27[34m" },
      { name = "debug", color = "\27[36m" },
      { name = "info", color = "\27[32m" },
      { name = "warn", color = "\27[33m" },
      { name = "error", color = "\27[31m" },
      { name = "fatal", color = "\27[35m" },
   }
   
   local levels = {}
   for i, v in ipairs(modes) do
      levels[v.name] = i
   end
   
   local round = function(x, increment)
      increment = increment or 1
      x = x / increment
      return (x > 0 and floor(x + 0.5) or ceil(x - 0.5)) * increment
   end
   
   local _tostring = tostring
   
   local tostring = function(...)
      local t = {}
      for i = 1, select("#", ...) do
         local x = select(i, ...)
         if type(x) == "number" then
            x = round(x, 0.01)
         end
         t[#t + 1] = _tostring(x)
      end
      return table.concat(t, " ")
   end
   
   for i, x in ipairs(modes) do
      local nameupper = x.name:upper()
      log[x.name] = function(...)
         -- Return early if we're below the log level
         if i < levels[log.level] then
            return
         end
   
         local msg = tostring(...)
         local info = debug.getinfo(2, "Sl")
         local lineinfo = info.short_src .. ":" .. info.currentline
   
         -- Output to console
         print(
            string.format(
               "%s[%-6s%s]%s %s: %s",
               log.usecolor and x.color or "",
               nameupper,
               os.date "%H:%M:%S",
               log.usecolor and "\27[0m" or "",
               lineinfo,
               msg
            )
         )
   
         -- Output to log file
         if log.outfile then
            local fp = io.open(log.outfile, "a")
            local str = string.format("[%-6s%s] %s: %s\n", nameupper, os.date(), lineinfo, msg)
            fp:write(str)
            fp:close()
         end
      end
   end
   
   -- general utilities
   
   function ut.is_array(tbl)
      return type(tbl) == "table" and (#tbl > 0 or next(tbl) == nil)
   end
   
   ---return size of hash table
   ---@param t table
   local tsize = function(t)
      local size = 0
      for _ in pairs(t) do
         size = size + 1
      end
      return size
   end
   
   ---structually compare two table, TODO: needed?
   ---@param rhs table
   ---@param lhs table
   ---@return boolean
   function ut.compare(rhs, lhs)
      if type(lhs) ~= type(rhs) then
         return false
      end
      if type(lhs) == "table" then
         if tsize(lhs) ~= tsize(rhs) then
            return false
         end
         for k, v in pairs(lhs) do
            local equal = ut.compare(v, rhs[k])
            if not equal then
               return false
            end
         end
      else
         return rhs == lhs
      end
      return true
   end
   
   ---@param value any
   ---@return string
   function ut.T(value)
      local base_type = type(value)
      if base_type == "table" then
         local cls = value.__class
         if cls then
            return cls
         end
      end
      return base_type
   end
   
   function ut.flatten(t)
      local flat = {}
      for i = 1, #t do
         local value = t[i]
         if ut.T(value) == "table" then
            local list = ut.flatten(value)
            for j = 1, #list do
               flat[#flat + 1] = list[j]
            end
         else
            flat[#flat + 1] = value
         end
      end
      return flat
   end
   
   ---list filter
   ---@param f function
   ---@param list table
   ---@return table
   function ut.filter(f, list)
      local res = {}
      for i = 1, #list do
         if f(list[i]) then
            res[#res + 1] = list[i]
         end
      end
      return res
   end
   
   local function reduce(f, acc, list)
      for i = 1, #list do
         acc = f(acc, list[i])
      end
      return acc
   end
   ut.reduce = reduce
   
   ---list map
   ---@param f function
   ---@param list table
   ---@return table
   function ut.map(f, list)
      for i = 1, #list do
         list[i] = f(list[i], i)
      end
      return list
   end
   
   ---dump table as key value pairs
   ---@param o table
   ---@return string
   function ut.tdump(o)
      if ut.T(o) == "table" then
         local s = {}
         for k, v in pairs(o) do
            s[#s + 1] = k
            s[#s + 1] = ": "
            s[#s + 1] = ut.tdump(v)
            s[#s + 1] = " "
         end
         return tconcat(s)
      else
         return tostring(Usecolor and ut.colors.red(o) or o)
      end
   end
   
   ---dump table of events the tidal way
   ---@param o table
   ---@return string
   function ut.dump(o)
      if ut.T(o) == "table" then
         local s = {}
         for k, v in pairs(o) do
            s[#s + 1] = Usecolor and ut.colors.cyan(k) or k
            s[#s + 1] = ": "
            s[#s + 1] = ut.dump(v)
            s[#s + 1] = (k ~= #o) and "\n" or ""
         end
         return tconcat(s)
      else
         return tostring(o)
      end
   end
   
   ---zip two list (xs, ys) with f(xs, ys)
   ---@param f function
   ---@param xs table
   ---@param ys table
   ---@return table
   function ut.zipWith(f, xs, ys)
      local acc = {}
      for i = 1, #xs do
         acc[i] = f(xs[i], ys[i])
      end
      return acc
   end
   
   ---concat two lists
   ---@param a table
   ---@param b table
   ---@return table
   local function concat(a, b)
      for i = 1, #b do
         a[#a + 1] = b[i]
      end
      -- return chain(a, b):totable()
      return a
   end
   ut.concat = concat
   
   ---concat two hashmaps
   ---@param a table
   ---@param b table
   ---@return table
   function ut.union(a, b)
      for k, v in pairs(b) do
         a[k] = v
      end
      return a
   end
   
   ---@param index number
   ---@param list table
   ---@return table, table
   local function splitAt(index, list)
      local fst, lst = {}, {}
      for k, v in pairs(list) do
         if k <= index then
            fst[#fst + 1] = v
         else
            lst[#lst + 1] = v
         end
      end
      return fst, lst
   end
   ut.splitAt = splitAt
   
   ---@param step number
   ---@param list table
   ---@return table
   function ut.rotate(step, list)
      local a, b = splitAt(step, list)
      return concat(b, a)
   end
   
   ---pipe fuctions: pipe(f, g, h)(x) -> f(g(h(x)))
   ---@param ... unknown
   ---@return unknown
   function ut.pipe(...)
      local funcs = { ... }
      return reduce(function(f, g)
         return function(...)
            return f(g(...))
         end
      end, ut.id, funcs)
   end
   
   local function reverse(...)
      local function reverse_h(acc, v, ...)
         if 0 == select("#", ...) then
            return v, acc()
         else
            return reverse_h(function()
               return v, acc()
            end, ...)
         end
      end
      return reverse_h(function() end, ...)
   end
   
   ---curry given function -> f(a, b, c) -> f(a)(b)(c)
   ---@param func function
   ---@param num_args number
   ---@return function
   local function curry(func, num_args)
      num_args = num_args or 2
      if num_args <= 1 then
         return func
      end
      local function curry_h(argtrace, n)
         if 0 == n then
            return func(reverse(argtrace()))
         else
            return function(onearg)
               return curry_h(function()
                  return onearg, argtrace()
               end, n - 1)
            end
         end
      end
      return curry_h(function() end, num_args)
   end
   ut.curry = curry
   
   ---flip two args of f
   ---@param f function
   ---@return function
   function ut.flip(f)
      return function(a, b)
         return f(b, a)
      end
   end
   
   local function xorwise(x)
      local a = bxor(lshift(x, 13), x)
      local b = bxor(rshift(a, 17), a)
      return bxor(lshift(b, 5), b)
   end
   
   local function _frac(x)
      return (x - x:floor()):asFloat()
   end
   
   local function timeToIntSeed(x)
      return xorwise(floor((_frac(x / 300) * 536870912)))
   end
   
   local function intSeedToRand(x)
      return (x % 536870912) / 536870912
   end
   
   function ut.timeToRand(x)
      return abs(intSeedToRand(timeToIntSeed(x)))
   end
   
   local nparams
   ---returns num_param, is_vararg
   ---@param func function
   ---@return number, boolean
   function nparams(func)
      local info = d_getinfo(func)
      return info.nparams, info.isvararg
   end
   if _VERSION == "Lua 5.1" and not jit then
      function nparams(func)
         local s = str_dump(func)
         assert(s:sub(1, 6) == "\27LuaQ\0", "This code works only in Lua 5.1")
         local int_size = s:byte(8)
         local ptr_size = s:byte(9)
         local pos = 14 + ptr_size + (s:byte(7) > 0 and s:byte(13) or s:byte(12 + ptr_size)) + 2 * int_size
         return s:byte(pos), s:byte(pos + 1) > 0
      end
   end
   ut.nparams = nparams
   
   ---register a f(..., pat) as a method for Pattern.f(self, ...), essentially switch the order of args
   ---@param f function
   ---@return function
   function ut.method_wrap(f)
      return function(...)
         local args = { ... }
         local pat = tremove(args, 1)
         args[#args + 1] = pat
         return f(unpack(args))
      end
   end
   
   ---if f gets less args then arity, then curry the f and pass the current amount of args into it
   ---@param arity number
   ---@param f function
   ---@return function
   function ut.curry_wrap(arity, f)
      return function(...)
         local args = { ... }
         if #args < arity then
            local cf = curry(f, arity)
            for _, v in ipairs(args) do
               cf = cf(v)
            end
            return cf
         else
            return f(...)
         end
      end
   end
   
   function ut.id(x)
      return x
   end
   
   ---for lua5.1 compatibility
   ---@param f any
   ---@param env any
   ---@return any
   function ut.setfenv(f, env)
      local i = 1
      while true do
         local name = d_getupvalue(f, i)
         if name == "_ENV" then
            d_setupvalue(f, i, env)
            break
         elseif not name then
            break
         end
         i = i + 1
      end
      return f
   end
   
   local function partition(array, left, right, pivotIndex)
      local pivotValue = array[pivotIndex]
      array[pivotIndex], array[right] = array[right], array[pivotIndex]
   
      local storeIndex = left
   
      for i = left, right - 1 do
         if array[i] <= pivotValue then
            array[i], array[storeIndex] = array[storeIndex], array[i]
            storeIndex = storeIndex + 1
         end
         array[storeIndex], array[right] = array[right], array[storeIndex]
      end
   
      return storeIndex
   end
   
   local function quicksort(array, left, right)
      if right > left then
         local pivotNewIndex = partition(array, left, right, left)
         quicksort(array, left, pivotNewIndex - 1)
         quicksort(array, pivotNewIndex + 1, right)
      end
   end
   ut.quicksort = quicksort
   
   function ut.get_args(f)
      local args = {}
      for i = 1, nparams(f) do
         args[#args + 1] = d_getlocal(f, i)
      end
      return args
   end
   if _VERSION == "Lua 5.1" and not jit then
      ut.get_args = function(f)
         local args = {}
         local hook = d_gethook()
   
         local argHook = function()
            local info = d_getinfo(3)
            if "pcall" ~= info.name then
               return
            end
   
            for i = 1, huge do
               local name = d_getlocal(2, i)
               if "(*temporary)" == name then
                  d_sethook(hook)
                  error ""
                  return
               end
               args[#args + 1] = name
            end
         end
   
         d_sethook(argHook, "c")
         pcall(f)
   
         return args
      end
   end
   
   function ut.getlocal(name, level)
      local value
      local found = false
   
      level = (level or 1) + 1
   
      for i = 1, huge do
         local n, v = d_getlocal(level, i)
         if not n then
            break
         end
         if n == name then
            value = v
            found = true
         end
      end
      if found then
         return value
      end
      -- try non-local variables
      local func = debug.getinfo(level, "f").func
      for i = 1, math.huge do
         local n, v = debug.getupvalue(func, i)
         if not n then
            break
         end
         if n == name then
            return v
         end
      end
   end
   
end

do
   
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
   
   function stream:notifyTick(cycleFrom, cycleTo, s, cps, bpc, mill, now)
      if not self.pattern then
         return
      end
      local events = self.pattern:onsetsOnly()(cycleFrom, cycleTo)
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
   
end

do
   -- Copyright (c) 2006-2013 Fabien Fleutot and others.
   a2s.__index = a2s
   
   local tconcat = table.concat
   local str_match = string.match
   local str_format = string.format
   local unpack = unpack or rawget(table, "unpack")
   
   -- TODO: check AST
   
   -- Instanciate a new AST->source synthetizer
   function a2s.new()
      local self = {
         _acc = {}, -- Accumulates pieces of source as strings
         current_indent = 0, -- Current level of line indentation
         indent_step = "   ", -- Indentation symbol, normally spaces or '\t'
      }
      return setmetatable(self, a2s)
   end
   
   --------------------------------------------------------------------------------
   -- Run a synthetizer on the `ast' arg and return the source as a string.
   -- Can also be used as a static method `M.run (ast)'; in this case,
   -- a temporary Metizer is instanciated on the fly.
   --------------------------------------------------------------------------------
   function a2s:run(ast)
      if not ast then
         self, ast = a2s.new(), self
      end
      self._acc = {}
      self:node(ast)
      return tconcat(self._acc)
   end
   
   --------------------------------------------------------------------------------
   -- Accumulate a piece of source file in the synthetizer.
   --------------------------------------------------------------------------------
   function a2s:acc(x)
      if x then
         self._acc[#self._acc + 1] = x
      end
   end
   
   --------------------------------------------------------------------------------
   -- Accumulate an indented newline.
   -- Jumps an extra line if indentation is 0, so that
   -- toplevel definitions are separated by an extra empty line.
   --------------------------------------------------------------------------------
   function a2s:nl()
      if self.current_indent == 0 then
         self:acc "\n"
      end
      self:acc("\n" .. self.indent_step:rep(self.current_indent))
   end
   
   --------------------------------------------------------------------------------
   -- Increase indentation and accumulate a new line.
   --------------------------------------------------------------------------------
   function a2s:nlindent()
      self.current_indent = self.current_indent + 1
      self:nl()
   end
   
   --------------------------------------------------------------------------------
   -- Decrease indentation and accumulate a new line.
   --------------------------------------------------------------------------------
   function a2s:nldedent()
      self.current_indent = self.current_indent - 1
      self:acc("\n" .. self.indent_step:rep(self.current_indent))
   end
   
   --------------------------------------------------------------------------------
   -- Keywords, which are illegal as identifiers.
   --------------------------------------------------------------------------------
   local keywords_list = {
      "and",
      "break",
      "do",
      "else",
      "elseif",
      "end",
      "false",
      "for",
      "function",
      "if",
      "in",
      "local",
      "nil",
      "not",
      "or",
      "repeat",
      "return",
      "then",
      "true",
      "until",
      "while",
   }
   local keywords = {}
   for _, kw in pairs(keywords_list) do
      keywords[kw] = true
   end
   
   --------------------------------------------------------------------------------
   -- Return true iff string `id' is a legal identifier name.
   --------------------------------------------------------------------------------
   local function is_ident(id)
      return str_match(id, "^[%a_][%w_]*$") and not keywords[id]
   end
   
   -- Return true iff ast represents a legal function name for
   -- syntax sugar ``function foo.bar.gnat() ... end'':
   -- a series of nested string indexes, with an identifier as
   -- the innermost node.
   local function is_idx_stack(ast)
      local tag = ast.tag
      if tag == "Index" then
         return is_idx_stack(ast[1])
      elseif tag == "Id" then
         return true
      else
         return false
      end
   end
   
   --------------------------------------------------------------------------------
   -- Operator precedences, in increasing order.
   -- This is not directly used, it's used to generate op_prec below.
   --------------------------------------------------------------------------------
   local op_preprec = {
      { "or", "and" },
      { "lt", "le", "eq", "ne" },
      { "concat" },
      { "add", "sub" },
      { "mul", "div", "mod" },
      { "unm", "unary", "not", "len" }, ---TODO:
      { "pow" },
      { "index" },
   }
   
   --------------------------------------------------------------------------------
   -- operator --> precedence table, generated from op_preprec.
   --------------------------------------------------------------------------------
   local op_prec = {}
   
   for prec, ops in ipairs(op_preprec) do
      for _, op in ipairs(ops) do
         op_prec[op] = prec
      end
   end
   
   --------------------------------------------------------------------------------
   -- operator --> source representation.
   --------------------------------------------------------------------------------
   local op_symbol = {
      add = " + ",
      sub = " - ",
      mul = " * ",
      div = " / ",
      mod = " % ",
      pow = " ^ ",
      concat = " .. ",
      eq = " == ",
      ne = " ~= ",
      lt = " < ",
      le = " <= ",
      ["and"] = " and ",
      ["or"] = " or ",
      ["not"] = "not ",
      len = "# ",
      unm = "-",
   }
   -- Accumulate the source representation of AST `node' in
   -- the synthetizer. Most of the work is done by delegating to
   -- the method having the name of the AST tag.
   -- If something can't be converted to normal sources, it's
   -- instead dumped as a `-{ ... }' splice in the source accumulator.
   function a2s:node(node)
      assert(self ~= a2s and self._acc, "wrong ast_to_src compiler?")
      if node == nil then
         self:acc "<<error>>"
         return
      end
      if not node.tag then -- tagless block.
         self:list(node, self.nl)
      else
         local f = a2s[node.tag]
         if type(f) == "function" then -- Delegate to tag method.
            f(self, node, unpack(node))
         elseif type(f) == "string" then -- tag string.
            self:acc(f)
         end
      end
   end
   
   --------------------------------------------------------------------------------
   -- Convert every node in the AST list `list' passed as 1st arg.
   -- `sep' is an optional separator to be accumulated between each list element,
   -- it can be a string or a synth method.
   -- `start' is an optional number (default == 1), indicating which is the
   -- first element of list to be converted, so that we can skip the begining
   -- of a list.
   --------------------------------------------------------------------------------
   function a2s:list(list, sep, start)
      for i = start or 1, #list do
         self:node(list[i])
         if list[i + 1] then
            if not sep then
               return -- HACK:
            elseif type(sep) == "function" then
               sep(self)
            elseif type(sep) == "string" then
               self:acc(sep)
            else
               error "Invalid list separator"
            end
         end
      end
   end
   
   --------------------------------------------------------------------------------
   --
   -- Tag methods.
   -- ------------
   --
   -- Specific AST node dumping methods, associated to their node kinds
   -- by their name, which is the corresponding AST tag.
   -- synth:node() is in charge of delegating a node's treatment to the
   -- appropriate tag method.
   --
   -- Such tag methods are called with the AST node as 1st arg.
   -- As a convenience, the n node's children are passed as args #2 ... n+1.
   --
   -- There are several things that could be refactored into common subroutines
   -- here: statement blocks dumping, function dumping...
   -- However, given their small size and linear execution
   -- (they basically perform series of :acc(), :node(), :list(),
   -- :nl(), :nlindent() and :nldedent() calls), it seems more readable
   -- to avoid multiplication of such tiny functions.
   --
   -- To make sense out of these, you need to know metalua's AST syntax, as
   -- found in the reference manual or in metalua/doc/ast.txt.
   --
   --------------------------------------------------------------------------------
   
   function a2s:Chunk(node)
      -- TODO: check ret last
      for _, v in ipairs(node) do
         self:node(v)
         self:acc "; "
      end
   end
   
   function a2s:Do(node)
      self:acc "do"
      self:nlindent()
      self:list(node, self.nl)
      self:nldedent()
      self:acc "end"
   end
   
   function a2s:Set(node)
      local lhs = node[1]
      local rhs = node[2]
      -- ``function foo:bar(...) ... end'' --
      if
         lhs[1].tag == "Index"
         and rhs[1].tag == "Function"
         and rhs[1][1][1] == "self"
         and is_idx_stack(lhs)
         and is_ident(lhs[1][2][1])
      then
         local method = lhs[1][2][1]
         local params = rhs[1][1]
         local body = rhs[1][2]
         self:acc "function "
         self:node(lhs)
         self:acc ":"
         self:acc(method)
         self:acc "("
         self:list(params, ", ", 2)
         self:acc ")"
         self:nlindent()
         self:list(body, self.nl)
         self:nldedent()
         self:acc "end"
      elseif rhs[1].tag == "Function" and is_idx_stack(lhs) then
         -- | `Set{ { lhs }, { `Function{ params, body } } } if is_idx_stack (lhs) ->
         -- ``function foo(...) ... end'' --
         local params = rhs[1][1]
         local body = rhs[1][2]
         self:acc "function "
         self:node(lhs)
         self:acc "("
         self:list(params, ", ")
         self:acc ")"
         self:nlindent()
         self:list(body, self.nl)
         self:nldedent()
         self:acc "end"
      else
         self:list(lhs, ", ")
         self:acc " = "
         self:list(rhs, ", ")
      end
   end
   
   function a2s:While(_, cond, body)
      self:acc "while "
      self:node(cond)
      self:acc " do"
      self:nlindent()
      self:list(body, self.nl)
      self:nldedent()
      self:acc "end"
   end
   
   function a2s:Repeat(_, body, cond)
      self:acc "repeat"
      self:nlindent()
      self:list(body, self.nl)
      self:nldedent()
      self:acc "until "
      self:node(cond)
   end
   
   function a2s:If(node)
      for i = 1, #node - 1, 2 do
         -- for each ``if/then'' and ``elseif/then'' pair --
         local cond, body = node[i], node[i + 1]
         self:acc(i == 1 and "if " or "elseif ")
         self:node(cond)
         self:acc " then"
         self:nlindent()
         self:list(body, self.nl)
         self:nldedent()
      end
      -- odd number of children --> last one is an `else' clause --
      if #node % 2 == 1 then
         self:acc "else"
         self:nlindent()
         self:list(node[#node], self.nl)
         self:nldedent()
      end
      self:acc "end"
   end
   
   function a2s:Fornum(node, var, first, last)
      local body = node[#node]
      self:acc "for "
      self:node(var)
      self:acc " = "
      self:node(first)
      self:acc ", "
      self:node(last)
      if #node == 5 then -- 5 children --> child #4 is a step increment.
         self:acc ", "
         self:node(node[4])
      end
      self:acc " do"
      self:nlindent()
      self:list(body, self.nl)
      self:nldedent()
      self:acc "end"
   end
   
   function a2s:Forin(_, vars, generators, body)
      self:acc "for "
      self:list(vars, ", ")
      self:acc " in "
      self:list(generators, ", ")
      self:acc " do"
      self:nlindent()
      self:list(body, self.nl)
      self:nldedent()
      self:acc "end"
   end
   
   function a2s:Local(_, lhs, rhs, annots)
      self:acc "local "
      if annots then
         local n = #lhs
         for i = 1, n do
            self:node(lhs)
            local a = annots[i]
            if a then
               self:acc " #"
               self:node(a)
            end
            if i ~= n then
               self:acc ", "
            end
         end
      else
         self:list(lhs, ", ")
      end
      if rhs[1] then
         self:acc " = "
         self:list(rhs, ", ")
      end
   end
   
   function a2s:Localrec(_, lhs, rhs)
      -- ``local function name() ... end'' --
      self:acc "local function "
      self:acc(lhs[1][1])
      self:acc "("
      self:list(rhs[1][1], ", ")
      self:acc ")"
      self:nlindent()
      self:list(rhs[1][2], self.nl)
      self:nldedent()
      self:acc "end"
   end
   
   function a2s:Call(node, f)
      local parens
      if node[2].tag == "String" or node[2].tag == "Table" then
         parens = false
      else
         parens = true
      end
      self:node(f)
      self:acc(parens and "(" or " ")
      self:list(node, ", ", 2) -- skip `f'.
      self:acc(parens and ")")
   end
   
   function a2s:Invoke(node, f, method)
      -- single string or table literal arg ==> no need for parentheses. --
      local parens
      if node[2].tag == "String" or node[2].tag == "Table" then
         parens = false
      else
         parens = true
      end
      self:node(f)
      self:acc ":"
      self:acc(method[1])
      self:acc(parens and "(" or " ")
      self:list(node, ", ", 3) -- Skip args #1 and #2, object and method name.
      self:acc(parens and ")")
   end
   
   function a2s:Return(node)
      self:acc "return "
      self:list(node, ", ")
   end
   
   a2s.Break = "break"
   a2s.Nil = "nil"
   a2s.False = "false"
   a2s.True = "true"
   a2s.Dots = "..."
   
   function a2s:Number(_, n)
      self:acc(tostring(n))
   end
   
   function a2s:String(_, str)
      -- format "%q" prints '\n' in an umpractical way IMO,
      -- so this is fixed with the :gsub( ) call.
      self:acc(str_format("%q", str):gsub("\\\n", "\\n"))
   end
   
   function a2s:Function(_, params, body, annots)
      self:acc "function("
      if annots then
         local n = #params
         for i = 1, n do
            local p, a = params[i], annots[i]
            self:node(p)
            if annots then
               self:acc " #"
               self:node(a)
            end
            if i ~= n then
               self:acc ", "
            end
         end
      else
         self:list(params, ", ")
      end
      self:acc ")"
      self:nlindent()
      self:list(body, self.nl)
      self:nldedent()
      self:acc "end"
   end
   
   function a2s:Table(node)
      if not node[1] then
         self:acc "{ }"
      else
         self:acc "{ "
         for i, elem in ipairs(node) do
            if elem.tag == "Pair" then
               -- `Pair{ `String{ key }, value }
               if elem[1].tag == "String" and is_ident(elem[1][1]) then
                  self:acc(elem[1][1])
                  self:acc " = "
                  self:node(elem[2])
               else
                  self:acc "["
                  self:node(elem[1])
                  self:acc "] = "
                  self:node(elem[2])
               end
            else
               self:node(elem)
            end
            if node[i + 1] then
               self:acc ", "
            end
         end
         self:acc " }"
      end
   end
   
   -- TODO: understand associatitivity
   function a2s:Op(node, op, a, b)
      if op == "not" and (node[2][1][1] == "eq") then ---TODO:???
         op, a, b = "ne", node[2][1][2], node[2][1][3]
      end
      if b then -- binary operator.
         local left_paren, right_paren
         if a.tag == "Op" and op_prec[op] >= op_prec[a[1]] then
            left_paren = true
         else
            left_paren = false
         end
         if b.tag == "Op" and op_prec[op] >= op_prec[b[1]] then
            right_paren = true
         else
            right_paren = false
         end
         self:acc(left_paren and "(")
         self:node(a)
         self:acc(left_paren and ")")
   
         self:acc(op_symbol[op])
   
         self:acc(right_paren and "(")
         self:node(b)
         self:acc(right_paren and ")")
      else -- unary operator.
         local paren
         if a.tag == "Op" and op_prec[op] >= op_prec[a[1]] then
            paren = true
         else
            paren = false
         end
         self:acc(op_symbol[op])
         self:acc(paren and "(")
         self:node(a)
         self:acc(paren and ")")
      end
   end
   
   function a2s:Paren(_, content)
      self:acc "("
      self:node(content)
      self:acc ")"
   end
   
   function a2s:Index(_, table, key)
      local paren_table
      if table.tag == "Op" and op_prec[table[1][1]] < op_prec.index then
         paren_table = true
      else
         paren_table = false
      end
   
      self:acc(paren_table and "(")
      self:node(table)
      self:acc(paren_table and ")")
   
      -- ``table [key]''
      if key.tag == "String" and is_ident(key[1]) then
         self:acc "."
         self:acc(key[1])
      else
         self:acc "["
         self:node(key)
         self:acc "]"
         -- ``table.key''
      end
   end
   
   function a2s:Id(_, name)
      if is_ident(name) then
         self:acc(name)
      else
         error "invalid identifier"
      end
   end
   
   function a2s:Goto(node, name)
      self:acc "goto "
      if type(name) == "string" then
         self:Id(node, name)
      else
         self:Id(node[1], node[1][1])
      end
   end
   
   function a2s:Label(node, name)
      self:acc "::"
      if type(name) == "string" then
         self:Id(node, name)
      else
         self:Id(node[1], node[1][1])
      end
      self:acc "::"
   end
   
end

do
   
   local P, S, V, R, C, Ct = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.C, lpeg.Ct
   
   
   local loadstring = ut.loadstring
   local setfenv = setfenv or ut.setfenv
   local memoize = ut.memoize
   local tremove = table.remove
   local ipairs = ipairs
   local type = type
   local filter = ut.filter
   local map = ut.map
   local reduce = ut.reduce
   local unpack = _G.unpack or rawget(table, "unpack")
   
   local sequence = V "sequence"
   local slice = V "slice"
   local sub_cycle = V "sub_cycle"
   local polymeter = V "polymeter"
   local slow_sequence = V "slow_sequence"
   local polymeter_steps = V "polymeter_steps"
   local stack = V "stack"
   local mini = V "mini"
   local op = V "op"
   local fast = V "fast"
   local slow = V "slow"
   local rand = V "rand"
   local replicate = V "replicate"
   local degrade = V "degrade"
   local weight = V "weight"
   local euclid = V "euclid"
   local tail = V "tail"
   local range = V "range"
   local list = V "list"
   local dollar = V "dollar"
   local tailop = V "tailop"
   local expr = V "expr"
   local ret = V "ret"
   local stat = V "stat"
   local choose = V "choose"
   local dotStack = V "dotStack"
   
   local function Id(a)
      return { tag = "Id", a }
   end
   
   local function Table(a)
      return { tag = "Table", unpack(a) }
   end
   
   local function Str(a)
      return { tag = "String", a }
   end
   
   local function Num(a)
      return { tag = "Number", a }
   end
   
   local function id(x)
      return x
   end
   
   local function Call(name, ...)
      return { tag = "Call", Id(name), ... }
   end
   
   local seed = -1 -- TODO: use this?
   local ws = S " \n\r\t" ^ 0
   local comma = ws * P "," * ws
   local pipe = ws * P "|" * ws
   local dot = ws * P "." * ws
   
   local function pNumber(num)
      return Num(tonumber(num))
   end
   
   local function pStep(chars)
      if chars == "~" then
         return Id "silence"
      elseif tonumber(chars) then
         return Num(tonumber(chars))
      -- elseif cache[chars] then
      --    return cache[chars]
      elseif chars:sub(0, 1) == "'" then
         return Id(chars:sub(2, #chars))
      end
      return Str(chars)
   end
   
   local function rTails(args)
      local f = tremove(args, 1)
      if f.tag == "String" then
         f.tag = "Id"
      end
      local params = filter(function(a)
         return type(a) ~= "function"
      end, args)
      local tails = filter(function(a)
         return type(a) == "function"
      end, args)
      local main = { tag = "Call", f, unpack(params) }
      for i = 1, #tails do
         main = tails[i](main)
      end
      return main
   end
   
   local step_char = R("09", "AZ", "az") + S [[~^'._]]
   local tidalop = (S "|+-*/^%><" ^ 2 + P "#") / id
   local arith = (S "+-*/^%" - P "|") / id
   local step = ws * ((step_char ^ 1 - P ".") / pStep) * ws
   local minus = P "-"
   local plus = P "+"
   local zero = P "0"
   local digit = R "09"
   local decimal_point = P "."
   local digit1_9 = R "19"
   local e = S "eE"
   local int = zero + (digit1_9 * digit ^ 0)
   local exp = e * (minus + plus) ^ -1 * digit ^ 1
   local frac = decimal_point * digit ^ 1
   local number = (minus ^ -1 * int * frac ^ -1 * exp ^ -1) / pNumber
   
   local function pFast(a)
      return function(x)
         return Call("fast", a, x)
      end
   end
   
   local function pSlow(a)
      return function(x)
         return Call("slow", a, x)
      end
   end
   
   -- local function pRand(a)
   --    lower = a[1] or 0
   --    return function(x)
   --       -- TODO: idea rand run
   --       return Num(math.random(lower, x[1]))
   --    end
   -- end
   
   local function pDegrade(a)
      if a == "?" then
         a = Num(0.5)
      end
      return function(x)
         seed = seed + 1
         return Call("degradeBy", a, x)
      end
   end
   
   local function pTail(b)
      return function(a)
         return Call("chain", a, b)
      end
   end
   
   local function pEuclid(p, s, r)
      r = r or Num(0)
      return function(x)
         return Call("euclidRot", p, s, r, x)
      end
   end
   
   local function pRange(s)
      return function(x)
         x.range = s[1]
         x.reps = nil
         return x
      end
   end
   
   local function pWeight(a)
      return function(x)
         x.weight = (x.weight or 1) + (tonumber(a[1]) or 2) - 1
         return x
      end
   end
   
   local function pReplicate(a)
      return function(x)
         x.reps = (x.reps or 1) + (tonumber(a[1]) or 2) - 1
         return x
      end
   end
   
   local function rReps(ast)
      local res = {}
      for _, node in ipairs(ast) do
         if node.reps then
            local reps = node.reps
            for _ = 1, reps do
               node.reps = nil
               res[#res + 1] = node
            end
         elseif node.range then
            for i = node[1], node.range do
               res[#res + 1] = Num(i)
            end
         else
            res[#res + 1] = node
         end
      end
      return res
   end
   
   local function pSlices(sli, ...)
      for _, v in ipairs { ... } do
         sli = v(sli)
      end
      return sli
   end
   
   local function addWeight(a, b)
      b = b.weight and b.weight or 1
      return a + b
   end
   
   local function rWeight(args)
      local acc = {}
      for _, v in ipairs(args) do
         acc[#acc + 1] = v.weight and Num(v.weight) or Num(1)
         acc[#acc + 1] = v
      end
      return acc
   end
   
   local function pSeq(isSlow)
      return function(args)
         local weightSum = reduce(addWeight, 0, args)
         if weightSum > #args then
            return Call(isSlow and "arrange" or "timecat", Table(rWeight(args)))
         else
            if #args == 1 then
               if isSlow then
                  return Call("pure", args[1])
               end
               return args[1]
            end
            return Call(isSlow and "slowcat" or "fastcat", Table(args))
         end
      end
   end
   
   local function pStack(...)
      local args = map(rReps, { ... })
      return rReps(args), "Stack"
   end
   
   local function pChoose(...)
      local args = map(rReps, { ... })
      return rReps(args), "Choose"
   end
   
   local function pDotStack(...)
      local args = map(rReps, { ... })
      return rReps(args), "DotStack"
   end
   
   local opsymb = {
      ["+"] = "add",
      ["-"] = "sub",
      ["*"] = "mul",
      ["/"] = "div",
      ["^"] = "pow",
      ["%"] = "mod",
      -- ["+"] = { "add", true },
      -- ["-"] = { "sub", true },
      -- ["*"] = { "mul", true },
      -- ["/"] = { "div", true },
      -- ["^"] = { "pow", true },
      -- ["%"] = { "mod", true },
      -- ["."] = { "pipe", false },
   }
   
   local function is_op(a)
      return opsymb[a]
   end
   
   local function pDollar(...)
      local args = { ... }
      if #args == 1 then
         return args
      end
      return rTails(args)
   end
   
   local function pList(...)
      local args = { ... }
      if is_op(args[1]) then
         local opname = opsymb[args[1]]
         if #args == 3 then
            tremove(args, 1)
            return { tag = "Op", opname, unpack(args) }
         elseif #args == 2 then
            return {
               tag = "Paren",
               { tag = "Function", { Id "x" }, { { tag = "Return", { tag = "Op", opname, Id "x", args[2] } } } },
            }
         else
            return args[1]
         end
      end
      return rTails(args)
   end
   
   local function pTailop(...)
      local args = { ... }
      local symb = tremove(args, 1)
      args = pDollar(unpack(args))
      return function(x)
         return { tag = "Call", { tag = "Index", Id "op", Str(symb) }, x, args }
      end
   end
   
   local function pSubCycle(args, tag)
      args = map(pSeq(false), args)
      if tag == "Stack" then
         return Call("stack", Table(args))
      elseif tag == "Choose" then
         return Call("randcat", Table(args))
      elseif tag == "DotStack" then
         return Call("fastcat", Table(args))
      end
   end
   
   local function pPolymeterSteps(s)
      return (s ~= "") and s or -1
   end
   
   local function pPolymeter(args, _, steps)
      steps = (steps == -1) and Num(#args[1]) or steps
      args = map(pSeq(false), args)
      return Call("polymeter", steps, Table(args))
   end
   
   local function pSlowSeq(args, tag)
      if tag then
         args = map(pSeq(false), args)
         if tag == "DotStack" then
            return Call("slowcat", Table(args))
         elseif tag == "Choose" then
            return Call("randcat", Table(args))
         end
      end
      return pSeq(true)(rReps(args))
   end
   
   local function pRoot(...)
      local stats = { ... }
      for i, a in ipairs(stats) do
         stats[i] = a
      end
      ---@diagnostic disable-next-line: inject-field
      stats.tag = "Chunk"
      return stats
   end
   
   local function pRet(a)
      return { tag = "Return", a }
   end
   
   local function pSet(lhs, rhs)
      lhs.tag = "Id"
      return { tag = "Set", { lhs }, { rhs } }
   end
   
   local function pStat(...)
      if select("#", ...) == 1 then
         return pRet { ... }
      end
      if select(2, ...) == "=" then
         return pSet(select(1, ...), select(3, ...))
      end
      return pRet(rTails { ... })
   end
   
   local function pDot(...)
      return { ... }
   end
   local tab = V "tab"
   
   local function pTab(...)
      return Table { ... }
   end
   
   local semi = P ";" ^ -1
   local grammar = {
      [1] = "root",
      root = (ret * semi) ^ 1 / pRoot,
      ret = (list + mini + dollar) / pRet,
      list = ws * P "(" * ws * (expr + arith) * expr ^ 0 * ws * P ")" * ws / pList,
      tab = ws * P "'(" * ws * expr ^ 1 * ws * P ")" * ws / pTab,
      dollar = S "$>" * ws * step * ws * expr ^ 0 * ws / pDollar,
      expr = ws * (tab + mini + list + dollar + tailop) * ws,
      sequence = (mini ^ 1) / pDot,
      stack = sequence * (comma * sequence) ^ 0 / pStack,
      choose = sequence * (pipe * sequence) ^ 1 / pChoose,
      dotStack = sequence * (dot * sequence) ^ 1 / pDotStack,
      tailop = tidalop * ws * step * ws * mini * ws / pTailop,
      mini = (slice * op ^ 0) / pSlices,
      slice = step + number + sub_cycle + polymeter + slow_sequence + list,
      sub_cycle = P "[" * ws * (dotStack + choose + stack) * ws * P "]" / pSubCycle,
      slow_sequence = P "<" * ws * (dotStack + choose + sequence) * ws * P ">" / pSlowSeq,
      polymeter = P "{" * ws * stack * ws * P "}" * polymeter_steps * ws / pPolymeter,
      polymeter_steps = (P "%" * slice) ^ -1 / pPolymeterSteps,
      -- op = fast + slow + tail + range + replicate + degrade + weight + euclid + rand,
      op = fast + slow + tail + range + replicate + degrade + weight + euclid,
      fast = P "*" * slice / pFast,
      slow = P "/" * slice / pSlow,
      tail = P ":" * slice / pTail,
      range = P ".." * ws * slice / pRange,
      -- rand = P "#" * (number ^ -1) / pRand,
      degrade = P "?" * (number ^ -1) / pDegrade,
      replicate = ws * P "!" * (number ^ -1) / pReplicate,
      weight = ws * (P "@" + P "_") * (number ^ -1) / pWeight,
      euclid = P "(" * ws * mini * comma * mini * ws * comma ^ -1 * mini ^ -1 * ws * P ")" / pEuclid,
   }
   
   local function make_gen(top_level)
      if top_level then
         stat = expr * (P "=" / id) ^ -1 * expr ^ 0 * ws / pStat
         grammar.root = (stat * semi) ^ 1 / pRoot
      else
         grammar.root = (ret * semi) ^ 1 / pRoot
      end
   
      local rules = Ct(C(grammar))
   
      local function read(str)
         return rules:match(str)[2]
      end
   
      return function(env)
         local to_str = function(src)
            local ok, ast
            ok, ast = pcall(read, src)
            if not ok then
               return false
            end
            local lua_src = a2s.run(ast) -- TODO: imporve api
            return lua_src
         end
   
         local function to_f(src)
            if not top_level then
               src = "[" .. src .. "]"
            end
            local ok, fn
            local lua_src = to_str(src)
            -- print(lua_src)
            if not lua_src then
               return false
            end
            ok, fn = pcall(loadstring, lua_src)
            if not ok then
               return false
            end
            setfenv(fn and fn or function()
               print "not a valid maxi notation"
            end, env)
            return fn()
         end
         return memoize(to_f)
      end
   end
   
   notation = { maxi = make_gen(true), mini = make_gen(false) }
   
end

do
   local P, S, V, R, C, Ct, Cc = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.C, lpeg.Ct, lpeg.Cc
   
   local concat, map = ut.concat, ut.map
   local qsort = ut.quicksort
   
   -- TODO: handle error for wrong chord names ...
   ---@enum (key) Chords
   local chordTable = {
      major = { 0, 4, 7 },
      aug = { 0, 4, 8 },
      six = { 0, 4, 7, 9 },
      sixNine = { 0, 4, 7, 9, 14 },
      major7 = { 0, 4, 7, 11 },
      major9 = { 0, 4, 7, 11, 14 },
      add9 = { 0, 4, 7, 14 },
      major11 = { 0, 4, 7, 11, 14, 17 },
      add11 = { 0, 4, 7, 17 },
      major13 = { 0, 4, 7, 11, 14, 21 },
      add13 = { 0, 4, 7, 21 },
      dom7 = { 0, 4, 7, 10 },
      dom9 = { 0, 4, 7, 14 },
      dom11 = { 0, 4, 7, 17 },
      dom13 = { 0, 4, 7, 21 },
      sevenFlat5 = { 0, 4, 6, 10 },
      sevenSharp5 = { 0, 4, 8, 10 },
      sevenFlat9 = { 0, 4, 7, 10, 13 },
      nine = { 0, 4, 7, 10, 14 },
      eleven = { 0, 4, 7, 10, 14, 17 },
      thirteen = { 0, 4, 7, 10, 14, 17, 21 },
      minor = { 0, 3, 7 },
      diminished = { 0, 3, 6 },
      minorSharp5 = { 0, 3, 8 },
      minor6 = { 0, 3, 7, 9 },
      minorSixNine = { 0, 3, 9, 7, 14 },
      minor7flat5 = { 0, 3, 6, 10 },
      minor7 = { 0, 3, 7, 10 },
      minor7sharp5 = { 0, 3, 8, 10 },
      minor7flat9 = { 0, 3, 7, 10, 13 },
      minor7sharp9 = { 0, 3, 7, 10, 15 },
      diminished7 = { 0, 3, 6, 9 },
      minor9 = { 0, 3, 7, 10, 14 },
      minor11 = { 0, 3, 7, 10, 14, 17 },
      minor13 = { 0, 3, 7, 10, 14, 17, 21 },
      minorMajor7 = { 0, 3, 7, 11 },
      one = { 0 },
      five = { 0, 7 },
      sus2 = { 0, 2, 7 },
      sus4 = { 0, 5, 7 },
      sevenSus2 = { 0, 2, 7, 10 },
      sevenSus4 = { 0, 5, 7, 10 },
      nineSus4 = { 0, 5, 7, 10, 14 },
      sevenFlat10 = { 0, 4, 7, 10, 15 },
      nineSharp5 = { 0, 1, 13 },
      minor9sharp5 = { 0, 1, 14 },
      sevenSharp5flat9 = { 0, 4, 8, 10, 13 },
      minor7sharp5flat9 = { 0, 3, 8, 10, 13 },
      elevenSharp = { 0, 4, 7, 10, 14, 18 },
      minor11sharp = { 0, 3, 7, 10, 14, 18 },
   }
   
   local alias = {
      major = { "maj", "M" },
      minor = { "min", "m" },
      aug = { "plus", "sharp5" },
      diminished = "dim",
      diminished7 = "dim7",
      one = "1",
      five = "5",
      six = "6",
      nine = "9", -- ?????
      eleven = "11",
      thirteen = "13",
   
      major7 = "maj7",
      major9 = "maj9",
      major11 = "maj11",
      major13 = "maj13",
   
      minor7 = { "min7", "m7" },
      minor9 = { "min9", "m9" },
      minor11 = { "min11", "m11" },
      minor13 = { "min13", "m13" },
   
      sixNine = { "six9", "sixby9", "6by9" },
   
      sevenFlat5 = "7f5",
      sevenSharp5 = "7s5",
      sevenFlat9 = "7f9",
      minorSharp5 = { "msharp5", "mS5" },
      minor6 = { "min6", "m6" },
      minorSixNine = { "minor69", "min69", "minSixNine", "m69", "mSixNine", "m6by9" },
   
      minor7flat5 = { "minor7f5", "min7flat5", "m7flat5", "m7f5" },
      minor7sharp5 = { "minor7s5", "min7sharp5", "m7sharp5", "m7s5" },
      minor7flat9 = { "minor7f9", "min7flat9", "m7flat9", "min7f9", "m7f9" },
      minor7sharp9 = { "minor7s9", "min7sharp9", "m7sharp9", "min7s9", "m7s9" },
      minor9sharp5 = { "minor9s5", "min9sharp5", "min9s5", "m9sharp5", "m9s5" },
      minor7sharp5flat9 = "m7sharp5flat9",
      minor11sharp = "m11s",
   
      sevenSus2 = "7sus2",
      sevenSus4 = "7sus4",
      nineSus4 = { "ninesus4", "9sus4" },
      sevenFlat10 = "7f10",
      nineSharp5 = { "9sharp5", "9s5" },
      sevenSharp5flat9 = "7s5f9",
      elevenSharp = "11s",
   
      minorMajor7 = { "minMaj7", "mmaj7" },
   }
   
   local alias_lookup = {}
   
   for k, v in pairs(alias) do
      if type(v) == "table" then
         for _, al in ipairs(v) do
            alias_lookup[al] = k
         end
      else
         alias_lookup[v] = k
      end
   end
   
   setmetatable(chordTable, {
      __index = function(t, k)
         return t[alias_lookup[k]]
      end,
   })
   
   local token = function(id)
      return Ct(Cc(id) * C(V(id)))
   end
   local note = token "note"
   local chordname = token "chordname"
   local chordmods = token "chordmods"
   local notename = token "notename"
   local notemods = token "notemods"
   local range = token "range"
   local open = token "open"
   local drop = token "drop"
   local invert = token "invert"
   local offset = token "offset"
   local octave = token "octave"
   local number = token "number"
   local sep = V "sep"
   
   local grammar = {
      [1] = "chord",
      chord = note * sep ^ -1 * chordname ^ -1 * chordmods ^ -1,
      note = notename * notemods ^ -1,
      chordname = R("az", "09") ^ 1,
      chordmods = (sep * (range + open + drop + invert)) ^ 0,
      notename = R "ag",
      notemods = offset ^ -1 * octave ^ -1,
      offset = S "sfn",
      octave = R "05",
      range = number,
      open = P "o",
      drop = P "d" * number,
      invert = P "i" * number,
      number = R "09",
      sep = P "'",
   }
   
   grammar = Ct(C(grammar))
   
   local notes = { c = 0, d = 2, e = 4, f = 5, g = 7, a = 9, b = 11 }
   
   open = function(chord)
      chord[1] = chord[1] - 12
      chord[3] = chord[3] - 12
      return chord
   end
   
   drop = function(n, chord)
      chord = qsort(chord)
      local index = #chord - (n - 1)
      chord[index] = chord[index] - 12
      return chord
   end
   
   invert = function(n, chord)
      chord = qsort(chord)
      for i = 1, n do
         local index = i % #chord
         if index == 0 then
            index = #chord
         end
         chord[index] = chord[index] + 12
      end
      return chord
   end
   
   range = function(n, chord)
      local new_tones = {}
      n = tonumber(n)
      if #chord > n then
         local acc = {}
         for i = 1, n < 0 and #chord + n or n do
            acc[i] = chord[i]
         end
         return acc
      else
         for i = #chord + 1, n do
            local index = i % #chord
            octave = math.ceil(i / #chord) - 1
            if index == 0 then
               index = #chord
            end
            local new_tone = chord[index] + (12 * octave)
            new_tones[#new_tones + 1] = new_tone
         end
         return concat(chord, new_tones)
      end
   end
   
   local parseChord = function(chord)
      if type(chord) == "number" then
         return chord
      end
      local ast = grammar:match(chord)
      if not ast then
         return false
      end
      notename = notes[ast[2][3][2]]
      offset = 0
      octave = 5
      if ast[2][4] ~= nil then
         local mods = ast[2][4]
         local _max_0 = #mods
         for _index_0 = 3, _max_0 < 0 and #mods + _max_0 or _max_0 do
            local mod = mods[_index_0]
            if mod[1] == "offset" then
               local _exp_0 = mod[2]
               if "s" == _exp_0 then
                  offset = 1
               elseif "f" == _exp_0 then
                  offset = -1
               else
                  offset = 0
               end
            end
            if mod[1] == "octave" then
               octave = tonumber(mod[2])
            end
         end
      end
      local rootnote = notename + offset + (octave - 5) * 12
      if ast[3][2] == "" then
         return rootnote
      end
      local chordtable = chordTable[ast[3][2]]
      chordtable = map(function(x)
         return x + rootnote
      end, chordtable)
      if ast[4][2] ~= "" then
         local _list_0 = ast[4]
         local _max_0 = #ast[4]
         for _index_0 = 3, _max_0 < 0 and #_list_0 + _max_0 or _max_0 do
            local mod = _list_0[_index_0]
            if mod[1] == "open" then
               chordtable = open(chordtable)
            end
            if mod[1] == "drop" then
               chordtable = drop(mod[3][2], chordtable)
            end
            if mod[1] == "range" then
               chordtable = range(mod[3][2], chordtable)
            end
            if mod[1] == "invert" then
               chordtable = invert(mod[3][2], chordtable)
            end
         end
      end
      return chordtable
   end
   
   ---@enum (key) Scales
   local scaleTable = {
      minPent = { 0, 3, 5, 7, 10 },
      majPent = { 0, 2, 4, 7, 9 },
      ritusen = { 0, 2, 5, 7, 9 },
      egyptian = { 0, 2, 5, 7, 10 },
      kumai = { 0, 2, 3, 7, 9 },
      hirajoshi = { 0, 2, 3, 7, 8 },
      iwato = { 0, 1, 5, 6, 10 },
      chinese = { 0, 4, 6, 7, 11 },
      indian = { 0, 4, 5, 7, 10 },
      pelog = { 0, 1, 3, 7, 8 },
      prometheus = { 0, 2, 4, 6, 11 },
      scriabin = { 0, 1, 4, 7, 9 },
      gong = { 0, 2, 4, 7, 9 },
      shang = { 0, 2, 5, 7, 10 },
      jiao = { 0, 3, 5, 8, 10 },
      zhi = { 0, 2, 5, 7, 9 },
      yu = { 0, 3, 5, 7, 10 },
      whole = { 0, 2, 4, 6, 8, 10 },
      augmented = { 0, 3, 4, 7, 8, 11 },
      augmented2 = { 0, 1, 4, 5, 8, 9 },
      hexMajor7 = { 0, 2, 4, 7, 9, 11 },
      hexDorian = { 0, 2, 3, 5, 7, 10 },
      hexPhrygian = { 0, 1, 3, 5, 8, 10 },
      hexSus = { 0, 2, 5, 7, 9, 10 },
      hexMajor6 = { 0, 2, 4, 5, 7, 9 },
      hexAeolian = { 0, 3, 5, 7, 8, 10 },
      major = { 0, 2, 4, 5, 7, 9, 11 },
      ionian = { 0, 2, 4, 5, 7, 9, 11 },
      dorian = { 0, 2, 3, 5, 7, 9, 10 },
      phrygian = { 0, 1, 3, 5, 7, 8, 10 },
      lydian = { 0, 2, 4, 6, 7, 9, 11 },
      mixolydian = { 0, 2, 4, 5, 7, 9, 10 },
      aeolian = { 0, 2, 3, 5, 7, 8, 10 },
      minor = { 0, 2, 3, 5, 7, 8, 10 },
      locrian = { 0, 1, 3, 5, 6, 8, 10 },
      harmonicMinor = { 0, 2, 3, 5, 7, 8, 11 },
      harmonicMajor = { 0, 2, 4, 5, 7, 8, 11 },
      melodicMinor = { 0, 2, 3, 5, 7, 9, 11 },
      melodicMinorDesc = { 0, 2, 3, 5, 7, 8, 10 },
      melodicMajor = { 0, 2, 4, 5, 7, 8, 10 },
      bartok = { 0, 2, 4, 5, 7, 8, 10 },
      hindu = { 0, 2, 4, 5, 7, 8, 10 },
      todi = { 0, 1, 3, 6, 7, 8, 11 },
      purvi = { 0, 1, 4, 6, 7, 8, 11 },
      marva = { 0, 1, 4, 6, 7, 9, 11 },
      bhairav = { 0, 1, 4, 5, 7, 8, 11 },
      ahirbhairav = { 0, 1, 4, 5, 7, 9, 10 },
      superLocrian = { 0, 1, 3, 4, 6, 8, 10 },
      romanianMinor = { 0, 2, 3, 6, 7, 9, 10 },
      hungarianMinor = { 0, 2, 3, 6, 7, 8, 11 },
      neapolitanMinor = { 0, 1, 3, 5, 7, 8, 11 },
      enigmatic = { 0, 1, 4, 6, 8, 10, 11 },
      spanish = { 0, 1, 4, 5, 7, 8, 10 },
      leadingWhole = { 0, 2, 4, 6, 8, 10, 11 },
      lydianMinor = { 0, 2, 4, 6, 7, 8, 10 },
      neapolitanMajor = { 0, 1, 3, 5, 7, 9, 11 },
      locrianMajor = { 0, 2, 4, 5, 6, 8, 10 },
      diminished = { 0, 1, 3, 4, 6, 7, 9, 10 },
      diminished2 = { 0, 2, 3, 5, 6, 8, 9, 11 },
      messiaen1 = { 0, 2, 4, 6, 8, 10 },
      messiaen2 = { 0, 1, 3, 4, 6, 7, 9, 10 },
      messiaen3 = { 0, 2, 3, 4, 6, 7, 8, 10, 11 },
      messiaen4 = { 0, 1, 2, 5, 6, 7, 8, 11 },
      messiaen5 = { 0, 1, 5, 6, 7, 11 },
      messiaen6 = { 0, 2, 4, 5, 6, 8, 10, 11 },
      messiaen7 = { 0, 1, 2, 3, 5, 6, 7, 8, 9, 11 },
      bayati = { 0, 1.5, 3, 5, 7, 8, 10 },
      hijaz = { 0, 1, 4, 5, 7, 8.5, 10 },
      sikah = { 0, 1.5, 3.5, 5.5, 7, 8.5, 10.5 },
      rast = { 0, 2, 3.5, 5, 7, 9, 10.5 },
      iraq = { 0, 1.5, 3.5, 5, 6.5, 8.5, 10.5 },
      saba = { 0, 1.5, 3, 4, 6, 8, 10 },
      chromatic = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 },
   }
   
   local floor = math.floor
   
   local getScale = function(name)
      return function(num)
         local istab = false
         if type(num) == "table" and num.note then
            num = num.note
            istab = true
         end
         num = tonumber(num)
         local scale = scaleTable[name]
         local index = (num + 1) % #scale
         local octave = floor(num / #scale)
         if index == 0 then
            index = #scale
         end
         local note = scale[index] + octave * 12
         if istab then
            return { ["note"] = note }
         end
         return note
      end
   end
   
   local flatten, zipWith, splitAt, rotate = ut.flatten, ut.zipWith, ut.splitAt, ut.rotate
   local min = math.min
   
   local function left(n, m)
      local ons, offs = n[1], n[2]
      local xs, ys = m[1], m[2]
      local _xs, __xs = splitAt(offs, xs)
      return { offs, ons - offs }, { zipWith(concat, _xs, ys), __xs }
   end
   
   local function right(n, m)
      local ons, offs = n[1], n[2]
      local xs, ys = m[1], m[2]
      local _ys, __ys = splitAt(ons, ys)
      return { ons, offs - ons }, { zipWith(concat, xs, _ys), __ys }
   end
   
   local function _bjork(n, m)
      local ons, offs = n[1], n[2]
      if min(ons, offs) <= 1 then
         return { n, m }
      else
         if ons > offs then
            return _bjork(left(n, m))
         else
            return _bjork(right(n, m))
         end
      end
   end
   
   local function bjork(ons, steps, offset)
      offset = offset or 0
      local offs = steps - ons
      local x, y = {}, {}
      for i = 1, ons do
         x[i] = { true }
      end
      for i = 1, offs do
         y[i] = { false }
      end
      local result = _bjork({ ons, offs }, { x, y })
      result = concat(flatten(result[2][1]), flatten(result[2][2]))
      return rotate(offset, result)
   end
   
   theory = { getScale = getScale, parseChord = parseChord, bjork = bjork }
   
end

do
   _G.struct = nil
   local Stream = types.Stream
   
   local floor = math.floor
   local type = type
   local pairs = pairs
   
   local sleep = function(sec)
      return socket.sleep(sec)
   end
   
   local target = {
      name = "SuperDirt",
      address = "127.0.0.1",
      port = 57120,
      latency = 0.2,
      handshake = true,
   }
   
   local typeMap = { table = "b", number = "f", string = "s" }
   
   local typesString = function(msg)
      local ts = ""
      for i = 1, #msg do
         local x = msg[i]
         if typeMap[type(x)] then
            ts = ts .. typeMap[type(x)]
         else
            ts = ts .. "b"
         end
      end
      return ts
   end
   
   local osc, sendOSC
   if has_losc then
      osc = losc.new {
         plugin = plugin.new {
            sendPort = target.port,
            sendAddr = target.address,
         },
      }
      sendOSC = function(value, ts)
         local msg = {}
         for key, val in pairs(value) do
            msg[#msg + 1] = key
            msg[#msg + 1] = val
         end
         msg.types = typesString(msg)
         msg.address = "/dirt/play"
         -- local b = osc.new_message(msg)
         local b = osc.new_bundle(ts, osc.new_message(msg))
         osc:send(b)
      end
   end
   
   local mt = { __class = "clock" }
   
   function mt:start()
      if not self.running then
         self.running = true
         return self:createNotifyCoroutine()
      end
   end
   
   function mt:stop()
      self.running = false
      print "Clock: stopped"
   end
   
   function mt:subscribe(key, pattern)
      if not self.subscribers[key] then
         self.subscribers[key] = Stream(self.callback)
      end
      self.subscribers[key].pattern = pattern
   end
   
   function mt:unsubscribe(key)
      self.subscribers[key] = nil
   end
   
   function mt:setbpm(bpm)
      self.sessionState:set_tempo(bpm, 0)
      self.link:commit_audio_session_state(self.sessionState)
   end
   
   function mt:setcps(cps)
      self.sessionState:set_tempo(cps * self.beatsPerCycle * 60, 0)
      self.link:commit_audio_session_state(self.sessionState)
   end
   
   function mt:createNotifyCoroutine()
      self.co = coroutine.create(function(f)
         local start = self.link:clock_micros()
         local ticks = 0
         local mill = 1000000
         local frame = self.sampleRate * mill
         while self.running do
            ticks = ticks + 1
            local logicalNow = floor(start + (ticks * frame))
            local logicalNext = floor(start + ((ticks + 1) * frame))
            local now = self.link:clock_micros()
            local wait = (logicalNow - now) / mill
            if wait > 0 then
               sleep(wait)
            end
            if not self.running then
               break
            end
            self.link:capture_audio_session_state(self.sessionState)
            local cps = (self.sessionState:tempo() / self.beatsPerCycle) / 60
            local cycleFrom = self.sessionState:beat_at_time(logicalNow, 0) / self.beatsPerCycle
            local cycleTo = self.sessionState:beat_at_time(logicalNext, 0) / self.beatsPerCycle
            -- print(string.format("cycleFrom : %d;  cycleTo : %d", cycleFrom, cycleTo))
            if f then
               f()
            end
            for _, sub in pairs(self.subscribers) do
               sub:notifyTick(cycleFrom, cycleTo, self.sessionState, cps, self.beatsPerCycle, mill, now)
            end
            coroutine.yield()
         end
         self.linkEnabled = false
      end)
   end
   
   mt.__index = mt
   
   function Clock(bpm, sampleRate, beatsPerCycle, callback)
      bpm = bpm or 120
      sampleRate = sampleRate or (1 / 20)
      beatsPerCycle = beatsPerCycle or 4
      callback = callback or sendOSC
      return setmetatable({
         callback = callback,
         bpm = bpm,
         sampleRate = sampleRate,
         beatsPerCycle = beatsPerCycle,
         link = has_al and al.create(bpm) or {}, -- HACK:
         sessionState = has_al and al.create_session_state() or {},
         subscribers = {},
         running = false,
         latency = 0.2,
      }, mt)
   end
   
end

do
   local DefaultClock = Clock()
   
   function factory.p(key, pattern)
      DefaultClock:subscribe(key, pattern)
      return pattern
   end
   
   -- TODO: cause server to freeze ...
   function factory._p(key)
      DefaultClock:unsubscribe(key)
   end
   
   factory.p_ = factory._p
   
   function factory.hush()
      for i, _ in pairs(DefaultClock.subscribers) do
         DefaultClock:unsubscribe(i)
      end
   end
   
   -- function M.panic()
   --    M.hush()
   --    once(s "superpanic")
   -- end
   -- panic :: Tidally => IO ()
   -- panic = hush >> once (sound "superpanic")
   
   for i = 1, 16 do
      if i <= 12 then
         factory["d" .. i] = function(a)
            return factory.p(i, a:orbit(i - 1))
         end
      else
         factory["d" .. i] = function(a)
            return factory.p(i, a)
         end
      end
      factory["_d" .. i] = function()
         return factory._p(i)
      end
      factory["d" .. i .. "_"] = function()
         return factory._p(i)
      end
   end
   
   factory.DefaultClock = DefaultClock
   
   function factory.setcps(cps)
      DefaultClock:setcps(cps)
   end
   
   function factory.setbpm(bpm)
      DefaultClock:setbpm(bpm)
   end
   
   factory.bpm = factory.setbpm
   factory.cps = factory.setcps
   
end

do
   control.genericParams = {
      { "s", "n", "gain" },
      { "cutoff", "resonance" },
      { "hcutoff", "hresonance" },
   
      { "delay", "delaytime", "delayfeedback" },
      { "room", "size" },
      { "bandf", "bandq" }, --bpenv
      "toArg",
      "from",
      "to",
      "accelerate",
      "amp",
      "attack",
      "bandq",
      "begin",
      "legato",
      "clhatdecay",
      "crush",
      "coarse",
      "channel",
      "cut",
      "cutoff",
      "cutoffegint",
      "decay",
      "delayfeedback",
      "delaytime",
      "detune",
      "djf",
      "dry",
      "end",
      "fadeTime",
      "fadeInTime",
      "freq",
      "gain",
      "gate",
      "hatgrain",
      "hold",
      "hresonance",
      "lagogo",
      "lclap",
      "lclaves",
      "lclhat",
      "lcrash",
      "leslie",
      "lrate",
      "lfodelay",
      "lfosync",
      "lock",
      "metatune",
      "mtranspose",
      "octaveR",
      "ophatdecay",
      "orbit",
      "pan",
      "panorient",
      "portamento",
      "sagogo",
      "semitone",
      "speed",
      "sustain",
      "unit",
      "voice",
      "modwheel",
      "tremolorate",
      "fshiftnote",
      "kcutoff",
      "octer",
      "octersub",
      "octersubsub",
      "ring",
      "ringf",
      "ringdf",
      "distort",
      "freeze",
      "xsdelay",
      "tsdelay",
      "real",
      "imag",
      "enhance",
      "partials",
      "comb",
      "smear",
      "scram",
      "binshift",
      "hbrick",
      "lbrick",
   }
   
   control.aliasParams = {
      s = "sound",
      note = "up",
      attack = "att",
      bandf = "bpf",
      bandq = "bpq",
      clhatdecay = "chdecay",
      cutoff = { "ctf", "lpf" },
      cutoffegint = "ctfg",
      delayfeedback = { "dfb", "delayfb" },
      delaytime = { "dt", "delayt" },
      detune = "det",
      fadeTime = "fadeOutTime",
      gate = "gat",
      hatgrain = "hg",
      hcutoff = "hpf",
      hresonance = "hpq",
      lagogo = "lag",
      lkick = "lbd",
      lclhat = "lch",
      lclaves = "lcl",
      lclap = "lcp",
      lcrash = "lcr",
      lfocutoffint = "lfoc",
      lfoint = "lfoi",
      lfopitchint = "lfop",
      lhitom = "lht",
      llotom = "llt",
      lophat = "loh",
      resonance = "lpq",
      lsnare = "lsn",
      n = "number",
      ophatdecay = "ohdecay",
      phaserdepth = "phasdp",
      phaserrate = "phasr",
      pitch1 = "pit1",
      pitch2 = "pit2",
      pitch3 = "pit3",
      portamento = "por",
      release = "rel",
      sagogo = "sag",
      sclaves = "scl",
      sclap = "scp",
      scrash = "scr",
      size = "sz",
      slide = "sld",
      stutterdepth = "std",
      stuttertime = "stt",
      sustain = "sus",
      tomdecay = "tdecay",
      tremolodepth = "tremdp",
      tremolorate = "tremr",
      vcfegint = "vcf",
      vcoegint = "vco",
      voice = "voi",
   }
   
end

do
   
   local bjork, getScale = theory.bjork, theory.getScale
   local Event, Span, Time, TDef, ValueMap = types.Event, types.Span, types.Time, types.TDef, types.ValueMap
   
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
   local is_array = ut.is_array
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
   local get_args = ut.get_args
   local timeToRand = ut.timeToRand
   local memoize = ut.memoize
   local T = ut.T
   
   local fast, pure, fastcat, slowcat, stack, silence, focus, range, rev, compress
   
   local TYPES = {}
   local op = {}
   
   -- give mini access to global vars
   setmetatable(pattern, { __index = _G })
   
   local eval = notation.mini(pattern)
   local reify = memoize(function(thing)
      local t = T(thing)
      if "string" == t then
         local res = eval(thing)
         return res and res or silence
      elseif "table" == t then
         if is_array(thing) then
            return fastcat(thing)
         else
            return pure(ValueMap(thing))
         end
      elseif "pattern" == t then
         return thing
      else
         return pure(thing)
      end
   end)
   pattern.reify = reify
   
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
   
   ---@class Pattern
   local function Pattern(query)
      query = query or function()
         return {}
      end
      return setmetatable({ query = query }, mt)
   end
   pattern.Pattern = Pattern
   
   local function querySpan(pat, b, e)
      local span = Span(b, e)
      -- local state = State(span)
      return setmetatable(pat.query(span), {
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
      local query = function(span)
         local cycles = span:spanCycles()
         local res = {}
         for i = 1, #cycles do
            local evs = pat.query(cycles[i])
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
      local query = function(span)
         return pat.query(f(span))
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
            return Event(whole_func(event_func.whole, event_val.whole), new_part, event_func.value(event_val.value))
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
            local event_vals = pat_val.query(whole)
            for _, event_val in ipairs(event_vals) do
               local new_whole = event_func.whole
               local new_part = event_func.part:sect(event_val.part)
               if new_part then
                  local new_value = event_func.value(event_val.value)
                  events[#events + 1] = Event(new_whole, new_part, new_value)
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
            local event_funcs = pat.query(whole)
            for _, event_func in ipairs(event_funcs) do
               local new_whole = event_val.whole
               local new_part = event_func.part:sect(event_val.part)
               if new_part then
                  local new_value = event_func.value(event_val.value)
                  events[#events + 1] = Event(new_whole, new_part, new_value)
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
            local evs = func(a.value).query(a.part)
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
            local innerPat = pattern.focus(span.start, span.stop, outerEvent.value)
            local innerEvents = innerPat.query(outerEvent.part)
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
      set = function(_, b) return b end,
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
   
   silence = Pattern()
   pattern.silence = silence
   
   function pure(value)
      local query = function(span)
         local cycles = span:spanCycles()
         for i, v in ipairs(cycles) do
            cycles[i] = Event(v.start:wholeCycle(), v, value)
         end
         return cycles
      end
      return Pattern(query)
   end
   pattern.pure = pure
   
   local function purify(value)
      if T(value) == "pattern" then
         return value
      else
         return pure(value)
      end
   end
   
   local function patternify(arity, func)
      return function(...)
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
         local f_p = patternify(arity, f)
         local f_p_t = type_wrap(f_p, name)
         local f_c_p_t = curry_wrap(arity, f_p_t)
         pattern[name] = f_c_p_t
         rawset(mt, name, method_wrap(f_p_t))
      else
         TYPES[name] = tdef
         local f_t = type_wrap(f, name)
         local f_t_c = curry_wrap(arity, f_t)
         pattern[name] = f_t_c
         rawset(mt, name, method_wrap(f_t))
      end
   end
   pattern.register = register
   
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
   
   function pattern.polymeter(steps, pats)
      for i, pat in ipairs(pats) do
         pats[i] = pattern.fast(steps / pat:len(), pat)
      end
      return stack(pats)
   end
   -- register("polymeter :: Pattern Int -> [Pattern a] -> Pattern a", polymeter, false)
   
   function slowcat(pats)
      local query = function(span)
         local cyc = span.start:sam():asFloat()
         local n = #pats
         local i = cyc % n
         local pat = pats[i + 1]
         if not pat then
            return {}
         end
         local offset = cyc - (cyc - i) / n
         return withEventTime(pat, function(t)
            return t + offset
         end).query(span:withTime(function(t)
            return t - offset
         end))
      end
      return splitQueries(Pattern(query))
   end
   register("slowcat :: [Pattern a] -> Pattern a", slowcat, false)
   
   function fastcat(pats)
      return pattern.fast(#pats, pattern.slowcat(pats))
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
   pattern.timecat = timecat
   
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
         tups[i + 1] = pattern.fast(cycles, pat)
      end
      return slow(total, timecat(tups))
   end
   pattern.arrange = arrange
   
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
      if factor:lte(0) then
         return silence
      end
      factor = factor:max(1)
      local mungeQuery = function(t)
         return t:sam() + ((t - t:sam()) * factor):min(1)
      end
      local eventSpanFunc = function(span)
         local b = span.start:sam() + (span.start - span.start:sam()) / factor
         local e = span.start:sam() + (span.stop - span.start:sam()) / factor
         return Span(b, e)
      end
      local query = function(span)
         local new_span = Span(mungeQuery(span.start), mungeQuery(span.stop))
         if new_span.start == new_span.start:nextSam() then
            return {}
         end
         local events = pat.query(new_span)
         for i = 1, #events do
            events[i] = events[i]:withSpan(eventSpanFunc)
         end
         return events
      end
      return splitQueries(Pattern(query))
   end
   register("fastgap :: Pattern Time -> Pattern a -> Pattern a", fastgap)
   
   function compress(b, e, pat)
      if b:gt(e) or e:gt(1) or b:gt(1) or b:lt(0) or e:lt(0) then
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
   
   local function segment(n, pat)
      return appLeft(fast(n, pure(id)), pat)
   end
   register("segment :: Pattern Time -> Pattern a -> Pattern a", segment)
   
   function range(mi, ma, pat)
      return pat * (ma - mi) + mi
   end
   register("range :: Pattern number -> Pattern number -> Pattern number -> Pattern a", range)
   
   local waveform = function(func)
      local query = function(span)
         return { Event(nil, span, func(span:midpoint())) }
      end
   
      return Pattern(query)
   end
   
   pattern.steady = function(value)
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
   
   -- stylua: ignore start
   local sine2 = waveform(function(t) return sin(t:asFloat() * pi * 2) end)
   local sine = fromBipolar(sine2)
   local cosine2 = late(1 / 4, sine2)
   local cosine = fromBipolar(cosine2)
   local square = waveform(function(t) return floor((t * 2) % 2) end)
   local square2 = toBipolar(square)
   local isaw = waveform(function(t) return -(t % 1) + 1 end)
   local isaw2 = toBipolar(isaw)
   local saw = waveform(function(t) return t % 1 end)
   local saw2 = toBipolar(saw)
   local tri = fastcat { isaw, saw }
   local tri2 = fastcat { isaw2, saw2 }
   local time = waveform(id)
   local rand = waveform(timeToRand)
   -- stylua: ignore end
   
   local _irand = function(i)
      return fmap(rand, function(x)
         return floor(x * i)
      end)
   end
   
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
      return chooseInWith(rand, vals)
   end
   
   local randcat = function(pats)
      return pattern.segment(1, choose(pats))
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
      return degradeByWith(rand, by, pat)
   end
   register("degradeBy :: Pattern Double -> Pattern a -> Pattern a", degradeBy)
   
   local function undegradeBy(by, pat)
      return degradeByWith(
         fmap(rand, function(r)
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
      local query = function(span)
         local cycle = span.start:sam()
         local nextCycle = span.start:nextSam()
         local reflect = function(to_reflect)
            local reflected = to_reflect:withTime(function(t)
               return cycle + (nextCycle - t)
            end)
            local tmp = reflected.start
            reflected.start = reflected.stop
            reflected.stop = tmp
            return reflected
         end
         local events = pat.query(reflect(span))
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
   
   local function echoWith(times, tim, f, pat)
      local acc = {}
      for i = 0, times - 1 do
         acc[i] = f(pattern.late(tim * i, pat))
      end
      return stack(acc)
   end
   register("echoWith :: Pattern Int -> Pattern Int -> Pattern f -> Pattern a -> Pattern a", echoWith)
   
   local function when(test, f, pat)
      local query = function(state)
         local cycle_idx = state.span.start:sam()
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
      local query = function(span)
         local index = span.start:sam():asFloat() % #pats + 1
         local pat = pats[index]
         return pat.query(span)
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
   
   -- CONTROLS
   local function juxBy(n, f, pat)
      n = n / 2
      local left = pattern.pan(0.5) - n + pat
      local right = pattern.pan(0.5) + n + pat
      return overlay(left, f(right))
   end
   -- "juxBy :: Pattern Double -> (Pattern ValueMap -> Pattern ValueMap) -> Pattern ValueMap -> Pattern ValueMap",
   register("juxBy :: Pattern Double -> Pattern f -> Pattern ValueMap -> Pattern ValueMap", juxBy)
   
   local function striate(n, pat)
      local pats = {}
      for i = 1, n do
         pats[i] = pat .. { ["begin"] = (i - 1) / n, ["end"] = i / n }
      end
      return fastcat(pats)
   end
   register("striate :: Pattern Int -> Pattern ValueMap -> Pattern ValueMap", striate)
   
   local function chop(n, pat)
      local func = function(p)
         local acc = {}
         for i = 1, n do
            acc[i] = union({ begin = (i - 1) / n, ["end"] = i / n }, p)
         end
         return fastcat(acc)
      end
      return pat:squeezeBind(func)
   end
   register("chop :: Pattern Int -> Pattern ValueMap -> Pattern ValueMap", chop)
   --
   -- register("slice", function(npat, ipat, opat)
   --    return npat:innerBind(function(n)
   --       return ipat:outerBind(function(i)
   --          return opat:outerBind(function(o)
   --             local begin
   --             if type(n) == table then
   --                begin = n[i]
   --             else
   --                begin = i / n
   --             end
   --             local _end
   --             if type(n) == table then
   --                _end = n[i + 1]
   --             else
   --                _end = (i + 1) / n
   --             end
   --             return pure(union(o, {
   --                begin = begin,
   --                ["end"] = _end,
   --                _slices = n,
   --             }))
   --          end)
   --       end)
   --    end)
   -- end)
   --
   -- register("splice", function(npat, ipat, opat)
   --    local sliced = M.slice(npat, ipat, opat)
   --    return sliced:withEvent(function(event)
   --       return event:withValue(function(value)
   --          local new_attri = {
   --             speed = tofloat(tofrac(1) / tofrac(value._slices) / event.whole:duration()) * (value.speed or 1),
   --             unit = "c",
   --          }
   --          return union(new_attri, value)
   --       end)
   --    end)
   -- end)
   --
   local function loopAt(factor, pat)
      print(pat)
      pat = pat .. pattern.speed(factor:reverse():asFloat())
      -- .. pattern.unit "c"
      print(pat)
      return slow(factor, pat)
   end
   register("loopAt :: Pattern Time -> Pattern ValueMap -> Pattern ValueMap", loopAt)
   
   local function fit(pat)
      return withEvent(pat, function(event)
         return event:withValue(function(value)
            return union(value, {
               speed = event.whole:duration():reverse():asFloat(),
               unit = "c",
            })
         end)
      end)
   end
   register("fit :: Pattern ValueMap -> Pattern ValueMap", fit)
   --
   -- register("legato", function(factor, pat)
   --    factor = tofrac(factor)
   --    return pat:withEventSpan(function(span)
   --       return Span(span._begin, (span._begin + span:duration() * factor))
   --    end)
   -- end)
   
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
            local start, stop = pos, pos + charFraction
            local matches = filter(function(event)
               return event.whole.start <= start and event.whole.stop >= stop
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
                  local isOnset = event.whole.start == start
                  local char = nil
                  if isOnset then
                     char = dump(event.value)
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
   pattern.drawLine = drawLine
   
   ---CONTROLS
   local parseChord = theory.parseChord
   local genericParams, aliasParams = control.genericParams, control.aliasParams
   
   ---@param name string
   local create = function(name)
      local withVal, f
      if type(name) == "table" then
         withVal = function(xs)
            if type(xs) == "table" then
               local acc = {}
               for i, x in ipairs(xs) do
                  acc[name[i]] = x
               end
               return ValueMap(acc)
            else
               return ValueMap { [name] = xs }
            end
         end
         f = function(args)
            return reify(args):fmap(withVal)
         end
         name = name[1]
      else
         f = function(arg)
            return reify { [name] = arg }
         end
      end
      pattern[name] = f
      mt[name] = function(self, arg)
         return self .. f(arg)
      end
   end
   
   for _, param in ipairs(genericParams) do
      create(param)
      if aliasParams[param] ~= nil then
         local alias = aliasParams[param]
         if type(alias) == "table" then
            for _, al in ipairs(alias) do
               pattern[al] = pattern[param]
               mt[al] = mt[param]
            end
         else
            pattern[alias] = pattern[param]
            mt[alias] = mt[param]
         end
      end
   end
   
   pattern.note = function(pat, arp)
      local function chordToStack(thing)
         if type(thing) == "string" then
            if type(parseChord(thing)) == "table" then
               local notes = parseChord(thing)
               return notes -- arp function
            end
            return thing
         elseif T(thing) == "pattern" then
            return thing
               :fmap(function(chord)
                  local notes = parseChord(chord)
                  return arp and fastcat(notes) or stack(notes)
               end)
               :outerJoin()
         else
            return reify(thing)
         end
      end
      local withVal = function(v)
         return ValueMap { note = v }
         -- return { note = v }
      end
      return chordToStack(pat):fmap(withVal)
   end
   
   pattern.n = pattern.note
   mt.note = function(self, arg)
      return self .. pattern.note(arg)
   end
   mt.n = mt.note
   
   pattern.op = op
   pattern.id = id
   pattern.T = T
   pattern.pipe = ut.pipe
   pattern.dump = ut.dump
   pattern.t = TYPES
   pattern.mt = mt
   pattern.tri2 = tri2
   pattern.tri = tri
   pattern.saw2 = saw2
   pattern.saw = saw
   pattern.isaw = isaw
   pattern.isaw2 = isaw2
   pattern.square2 = square2
   pattern.square = square
   pattern.cosine = cosine
   pattern.cosine2 = cosine2
   pattern.sine = sine
   pattern.sine2 = sine2
   pattern.rand = rand
   pattern.time = time
   
end

   local mt = pattern.mt
   
   local modal = {}
   modal.version = "modal dev-1"
   modal.url = "https://github.com/noearc/modal"
   
   local pairs = pairs
   
   modal.Clock = Clock
   
   for name, func in pairs(notation) do
      modal[name] = func
   end
   
   for name, func in pairs(theory) do
      modal[name] = func
   end
   
   for name, func in pairs(factory) do
      modal[name] = func
      mt[name] = ut.method_wrap(func)
   end
   
   for name, func in pairs(types) do
      modal[name] = func
   end
   
   for name, func in pairs(pattern) do
      modal[name] = func
   end
   
   setmetatable(modal, {
      __index = _G,
   })
   
   setmetatable(modal, {
      __call = function(t, override)
         for k, v in pairs(t) do
            if _G[k] ~= nil then
               local msg = "function " .. k .. " already exists in global scope."
               print("WARNING: " .. msg)
               if override then
                  _G[k] = v
                  print("WARNING: " .. msg .. " Overwritten.")
               end
            else
               _G[k] = v
            end
         end
      end,
   })
   
do
   local function repl()
      local host = "localhost"
      local port = 9000
      local maxi = notation.maxi(modal)
   
      local keywords = {}
      for i, _ in pairs(modal) do
         keywords[#keywords + 1] = i
      end
   
      if has_RL then
         RL.set_complete_list(keywords)
         RL.set_options { keeplines = 1000, histfile = "~/.synopsis_history" }
         RL.set_readline_name "modal"
      end
   
      local ok, c = pcall(socket.connect, host, port)
   
      local optf = {
         ["?"] = function()
            return [[
   :v  show _VERSION
   :t  get type for lib func (TODO: for expression)
   :q  quit repl ]]
         end,
         t = function(a)
            return tostring(modal.t[a])
         end,
         v = function()
            return modal._VERSION
         end,
         -- info = function(name)
         --    return dump(doc[name])
         -- end,
         q = function()
            if c then
               c:close()
            end
            os.exit()
         end,
      }
   
      -- TODO: see luaish, first run as lua with multiline? no ambiguiaty?>
      local eval = function(a)
         if a:sub(1, 1) == ":" then
            local name, param = a:match "(%a+)%s(%a*)"
            name = name and name or a:sub(2, #a)
            param = param and param or nil
            return optf[name](param)
         else
            local fn = maxi(a)
            return fn
         end
      end
   
      local function readline(a)
         io.write(a)
         return io.read()
      end
   
      local read = has_RL and RL.readline or readline
   
      local line
      print "modal repl   :? for help"
      while true do
         line = read "> "
         if line == "exit" then
            if c then
               c:close()
            end
            break
         end
   
         if line ~= "" then
            local res = eval(line)
            if res then
               print(res)
            end
            if has_RL then
               RL.add_history(line)
               -- RL.save_history()
            end
            if c then
               c:send(line .. "\n")
            end
         end
      end
   
      c:close()
      os.exit()
   end
   modal.repl = repl
   
end

do
   local function server()
      local maxi = notation.maxi(modal)
      local log = ut.log
   
      local clock = modal.DefaultClock
      clock:start()
   
      local host = "*"
      local port = arg[1] or 9000
      local sock = assert(socket.bind(host, port))
      local i, p = sock:getsockname()
      assert(i, p)
   
      print("Waiting connection from repl on " .. i .. ":" .. p .. "...")
      local c = assert(sock:accept())
      c:settimeout(0)
   
      print "Connected"
   
      local eval = function(a)
         local ok, fn = pcall(maxi, a)
         if not ok then
            log.warn("syntax error: " .. fn)
         else
            print(fn)
         end
      end
   
      local l, e
   
      local listen = function()
         l, e = c:receive()
         if not e then
            eval(l)
         end
      end
   
      repeat
         coroutine.resume(clock.co, listen)
      until false
   end
   
   modal.server = server
   
end

modal.ut = ut
return modal
