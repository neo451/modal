local ut = {}
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

return ut
