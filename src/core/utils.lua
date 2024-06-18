local fun = require "modal.fun"
local bit = require "modal.bitop"
local M = {}

local pairs = pairs
local ipairs = ipairs
local tostring = tostring
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

local tsize = function(t)
   local size = 0
   for _ in pairs(t) do
      size = size + 1
   end
   return size
end

M.compare = function(rhs, lhs)
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

local lua = { type = type }

-- local is_object = function(value)
--    return lua.type(value) == "table" and value.__class
-- end

-- M.T = function(obj)
--    return is_object(obj) and obj.__class or lua.type(obj)
-- end

M.T = function(value)
   local base_type = lua.type(value)
   if base_type == "table" then
      local cls = value.__class
      if cls then
         return cls
      end
   end
   return base_type
end

M.flatten = function(t)
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

M.filter = function(func, tab)
   return fun.filter(func, tab):totable()
end

M.map = function(func, tab)
   return fun.map(func, tab):totable()
end

-- TODO: color!
M.tdump = function(o)
   if M.T(o) == "table" then
      local s = { "{" }
      for k, v in pairs(o) do
         s[#s + 1] = " "
         s[#s + 1] = M.colors.red(k)
         s[#s + 1] = ": "
         s[#s + 1] = M.tdump(v)
      end
      s[#s + 1] = " } "
      return tconcat(s)
   else
      return tostring(o)
   end
end

M.dump = function(o)
   if M.T(o) == "table" then
      local s = {}
      for k, v in pairs(o) do
         s[#s + 1] = M.colors.red(k)
         s[#s + 1] = ": "
         s[#s + 1] = M.dump(v)
         s[#s + 1] = (k ~= #o) and "\n" or ""
      end
      return tconcat(s)
   else
      return tostring(o)
   end
end

M.zipWith = function(f, xs, ys)
   local acc = {}
   for i = 1, #xs do
      acc[i] = f(xs[i], ys[i])
   end
   return acc
end

M.concat = function(a, b)
   return fun.chain(a, b):totable()
end

M.union = function(a, b)
   return fun.chain(a, b):tomap()
end

M.splitAt = function(index, list)
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

M.rotate = function(step, list)
   local a, b = M.splitAt(step, list)
   return M.concat(b, a)
end

M.pipe = function(...)
   local funcs = { ... }
   return fun.reduce(function(f, g)
      return function(...)
         return f(g(...))
      end
   end, M.id, funcs)
end

M.curry = function(func, num_args)
   num_args = num_args or 2
   if num_args <= 1 then
      return func
   end
   local function curry_h(argtrace, n)
      if 0 == n then
         return func(M.reverse(argtrace()))
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

M.reverse = function(...)
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

M.flip = function(f)
   return function(a, b)
      return f(b, a)
   end
end

local xorwise = function(x)
   local a = bit.bxor(bit.lshift(x, 13), x)
   local b = bit.bxor(bit.rshift(a, 17), a)
   return bit.bxor(bit.lshift(b, 5), b)
end

local _frac = function(x)
   return (x - x:floor()):asFloat()
end

local timeToIntSeed = function(x)
   return xorwise(floor((_frac(x / 300) * 536870912)))
end

local intSeedToRand = function(x)
   return (x % 536870912) / 536870912
end

M.timeToRand = function(x)
   return abs(intSeedToRand(timeToIntSeed(x)))
end

M.string_lambda = function(env)
   return function(f)
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
         -- print(op, param, arg)
         if not (op or param or arg) then
            return error "not a string lambda"
         end
         local body = str_format("op['%s'](x, %s(%s))", op, param, arg)
         local fstr = "return function(x) return " .. body .. " end"
         -- print(fstr)
         local fn, err = loadstring(fstr)
         if not fn then
            return error(err)
         end
         fn = fn()
         setfenv(fn, env)
         return fn
      end
   end
end

-- TODO:
M.memoize = function(func)
   local cache = {}
   return function(k)
      local res = cache[k]
      if res == nil then
         res = func(k)
         cache[k] = res
      end
      return res
   end
end
-- TODO:
-- M.string_lambda = M.memoize(_string_lambda)

---returns num_param, is_vararg
---@param func function
---@return number, boolean
M.nparams = function(func)
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

function M.method_wrap(f)
   return function(...)
      local args = { ... }
      local pat = tremove(args, 1)
      args[#args + 1] = pat
      return f(unpack(args))
   end
end

---@param arity number
---@param f function
---@return function
function M.auto_curry(arity, f)
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

return M
