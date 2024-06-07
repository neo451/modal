local fun = require "modal.fun"
local bit = require "modal.bitop"
local M = {}
local floor = math.floor
local abs = math.abs
local tsize

tsize = function(t)
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

local lua = {
   debug = debug,
   type = type,
}

-- local is_object = function(value)
-- 	return lua.type(value) == "table" and value.__class
-- end
local is_object = require("moon").is_object

M.type = function(obj)
   return is_object(obj) and obj:type() or lua.type(obj)
end

M.flatten = function(t)
   local flat = {}
   for i = 1, #t do
      local value = t[i]
      if type(value) == "table" then
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
   return fun.totable(fun.filter(func, tab))
end

M.map = function(func, table)
   return fun.totable(fun.map(func, table))
end

M.reduce = fun.reduce

M.dump = function(o)
   if M.type(o) == "table" then
      local s = "{"
      for k, v in pairs(o) do
         s = s .. " " .. k .. ": " .. M.dump(v)
      end
      return s .. " } "
   else
      return tostring(o)
   end
end

M.dump2 = function(o)
   if M.type(o) == "table" then
      local s = ""
      for _, v in pairs(o) do
         s = s .. M.dump(v) .. "\n"
      end
      return s
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
   return fun.totable(fun.chain(a, b))
end

M.union = function(a, b)
   return fun.tomap(fun.chain(a, b))
end

M.splitAt = function(index, list)
   local fst, lst = {}, {}
   -- local pred = function(a)
   --    return a <= index
   -- end
   -- for _, k, v in fun.zip(fun.partition(pred, fun.iter(list))) do
   --    fst[#fst + 1] = k
   --    lst[#lst + 1] = v
   -- end
   -- require("moon.all")
   -- p(index)
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

M.id = function(x)
   return x
end

M.pipe = function(...)
   local funcs = { ... }
   return fun.reduce(function(f, g)
      return function(...)
         return f(g(...))
      end
   end, M.id, funcs)
end

M.rurry = function(func, num_args)
   num_args = num_args or 2
   if num_args <= 1 then
      return func
   end
   local function curry_h(argtrace, n)
      if 0 == n then
         return func(argtrace())
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
   local reverse_h
   reverse_h = function(acc, v, ...)
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
      if type(f) == "function" then
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
         local op, param, arg = string.match(f, "([%+%-%*|]*)%s*(%S+)%s*(%S+)")
         -- print(op, param, arg)
         if not (op or param or arg) then
            return error "not a string lambda"
         end
         local body = string.format("op['%s'](x, %s(%s))", op, param, arg)
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
      local s = string.dump(func)
      assert(s:sub(1, 6) == "\27LuaQ\0", "This code works only in Lua 5.1")
      local int_size = s:byte(8)
      local ptr_size = s:byte(9)
      local pos = 14 + ptr_size + (s:byte(7) > 0 and s:byte(13) or s:byte(12 + ptr_size)) + 2 * int_size
      return s:byte(pos), s:byte(pos + 1) > 0
   else
      local info = debug.getinfo(func)
      return info.nparams, info.isvararg
   end
end

local bind_methods = function(obj)
   return setmetatable({}, {
      __index = function(self, name)
         local val = obj[name]
         if val and type(val) == "function" then
            local bound
            bound = function(...)
               return val(obj, ...)
            end
            self[name] = bound
            return bound
         else
            return val
         end
      end,
   })
end

function M.method_wrap(f)
   return function(...)
      local args = { ... }
      local pat = table.remove(args, 1)
      table.insert(args, pat)
      return f(unpack(args))
   end
end

return M
