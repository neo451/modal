local fun = require "modal.fun"
local M = {}

-- TODO: memoize?

local pairs = pairs
local ipairs = ipairs
local tostring = tostring
local loadstring = loadstring or load
local type = type
local unpack = unpack or table.unpack
local setmetatable = setmetatable
local str_dump = string.dump
local str_match = string.match
local str_format = string.format
local str_char = string.char
local tconcat = table.concat
local tremove = table.remove
local floor = math.floor
local abs = math.abs
local debug_info = debug.getinfo

local reduce = fun.reduce
local map = fun.map
local filter = fun.filter
local chain = fun.chain

local setfenv = setfenv or M.setfenv

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

M.colors = colors

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
   local bitop
   bitop = function(a, b)
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

bit.tobit = function(x)
   return x % 2 ^ 32
end

bit.bxor = make_bitop {
   [0] = {
      [0] = 0,
      [1] = 1,
   },
   [1] = {
      [0] = 1,
      [1] = 0,
   },
   n = 4,
}

bit.bnot = function(a)
   return MODM - a
end

bit.band = function(a, b)
   return ((a + b) - bit.bxor(a, b)) / 2
end
bit.bor = function(a, b)
   return MODM - bit.band(MODM - a, MODM - b)
end

bit.rshift = function(a, disp)
   if disp < 0 then
      return bit.lshift(a, -disp)
   end
   return floor(a % 2 ^ 32 / 2 ^ disp)
end

bit.lshift = function(a, disp)
   if disp < 0 then
      return bit.rshift(a, -disp)
   end
   return (a * 2 ^ disp) % 2 ^ 32
end

M.bit = bit

-- general utilities

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
function M.compare(rhs, lhs)
   if type(lhs) ~= type(rhs) then
      return false
   end
   if type(lhs) == "table" then
      if tsize(lhs) ~= tsize(rhs) then
         return false
      end
      for k, v in pairs(lhs) do
         local equal = M.compare(v, rhs[k])
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
function M.T(value)
   local base_type = type(value)
   if base_type == "table" then
      local cls = value.__class
      if cls then
         return cls
      end
   end
   return base_type
end

function M.flatten(t)
   local flat = {}
   for i = 1, #t do
      local value = t[i]
      if M.T(value) == "table" then
         local list = M.flatten(value)
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
function M.filter(f, list)
   return filter(f, list):totable()
end

---list map
---@param f function
---@param list table
---@return table
function M.map(f, list)
   return map(f, list):totable()
end

---dump table as key value pairs
---@param o table
---@return string
function M.tdump(o)
   if M.T(o) == "table" then
      local s = {}
      for k, v in pairs(o) do
         s[#s + 1] = k
         s[#s + 1] = ": "
         s[#s + 1] = M.tdump(v)
         s[#s + 1] = " "
      end
      return tconcat(s)
   else
      return tostring(M.colors.red(o))
   end
end

---dump table of events the tidal way
---@param o table
---@return string
function M.dump(o)
   if M.T(o) == "table" then
      local s = {}
      for k, v in pairs(o) do
         s[#s + 1] = M.colors.cyan(k)
         s[#s + 1] = ": "
         s[#s + 1] = M.dump(v)
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
function M.zipWith(f, xs, ys)
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
function M.concat(a, b)
   return chain(a, b):totable()
end

---concat two hashmaps
---@param a table
---@param b table
---@return table
function M.union(a, b)
   return chain(a, b):tomap()
end

---@param index number
---@param list table
---@return table, table
function M.splitAt(index, list)
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

---@param step number
---@param list table
---@return table
function M.rotate(step, list)
   local a, b = M.splitAt(step, list)
   return M.concat(b, a)
end

---pipe fuctions: pipe(f, g, h)(x) -> f(g(h(x)))
---@param ... unknown
---@return unknown
function M.pipe(...)
   local funcs = { ... }
   return reduce(function(f, g)
      return function(...)
         return f(g(...))
      end
   end, M.id, funcs)
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
function M.curry(func, num_args)
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

---flip two args of f
---@param f function
---@return function
function M.flip(f)
   return function(a, b)
      return f(b, a)
   end
end

local function xorwise(x)
   local a = bit.bxor(bit.lshift(x, 13), x)
   local b = bit.bxor(bit.rshift(a, 17), a)
   return bit.bxor(bit.lshift(b, 5), b)
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

function M.timeToRand(x)
   return abs(intSeedToRand(timeToIntSeed(x)))
end

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
M.memoize = memoize

---turn string in format of "x -> body" to function(x) return body end | or the tidal preifx function calling syntax like "+| note 1"
---@param env table
---@return function
function M.string_lambda(env)
   return memoize(function(f)
      if type(f) == "function" or M.T(f) == "pattern" then
         return f
      end
      if f:find "->" then
         local arg, body = f:match "%s*(%S+)%s*%-%>%s*(.+)"
         local fstr = "return function(" .. arg .. ") return " .. body .. " end"
         local fn, err = loadstring(fstr)
         if not fn then
            return error(err)
         end
         fn = fn()
         setfenv(fn, env)
         return fn
      else
         local op, param, arg = str_match(f, "([%+%-%*|]*)%s*(%S+)%s*(%S+)")
         if not (op or param or arg) then
            return error "not a string lambda"
         end
         local body = str_format("op['%s'](x, %s(%s))", op, param, arg)
         local fstr = "return function(x) return " .. body .. " end"
         local fn, err = loadstring(fstr)
         if not fn then
            return false
         end
         fn = fn()
         setfenv(fn, env)
         return fn
      end
   end)
end

M.loadstring = memoize(loadstring)

---returns num_param, is_vararg
---@param func function
---@return number, boolean
function M.nparams(func)
   if _VERSION == "Lua 5.1" and not jit then
      local s = str_dump(func)
      assert(s:sub(1, 6) == "\27LuaQ\0", "This code works only in Lua 5.1")
      local int_size = s:byte(8)
      local ptr_size = s:byte(9)
      local pos = 14 + ptr_size + (s:byte(7) > 0 and s:byte(13) or s:byte(12 + ptr_size)) + 2 * int_size
      return s:byte(pos), s:byte(pos + 1) > 0
   else
      local info = debug_info(func)
      return info.nparams, info.isvararg
   end
end

---register a f(..., pat) as a method for Pattern.f(self, ...), essentially switch the order of args
---@param f function
---@return function
function M.method_wrap(f)
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
function M.curry_wrap(arity, f)
   return function(...)
      local args = { ... }
      if #args < arity then
         local cf = M.curry(f, arity)
         for _, v in ipairs(args) do
            cf = cf(v)
         end
         return cf
      else
         return f(...)
      end
   end
end

function M.id(x)
   return x
end

---for lua5.1 compatibility
---@param f any
---@param env any
---@return any
function M.setfenv(f, env)
   local i = 1
   while true do
      local name = debug.getupvalue(f, i)
      if name == "_ENV" then
         debug.setupvalue(f, i, env)
         break
      elseif not name then
         break
      end
      i = i + 1
   end
   return f
end

--- Iterize
--- debug in 51
-- function M.get_args(f)
--    local args = {}
--    for i = 1, M.nparams(f) do
--       table.insert(args, debug.getlocal(f, i))
--    end
--    return args
-- end

function M.get_args(f)
   local args = {}
   local hook = debug.gethook()

   local argHook = function()
      local info = debug.getinfo(3)
      if "pcall" ~= info.name then
         return
      end

      for i = 1, math.huge do
         local name = debug.getlocal(2, i)
         if "(*temporary)" == name then
            debug.sethook(hook)
            error ""
            return
         end
         table.insert(args, name)
      end
   end

   debug.sethook(argHook, "c")
   pcall(f)

   return args
end

return M
