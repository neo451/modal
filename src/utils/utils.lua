local is_object
is_object = require("moon.all").is_object
local fun = require("xi.fun")
local bit = require("xi.bitop")
local utils = { }
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
utils.compare = function(rhs, lhs)
  if type(lhs) ~= type(rhs) then
    return false
  end
  if type(lhs) == "table" then
    if tsize(lhs) ~= tsize(rhs) then
      return false
    end
    for k, v in pairs(lhs) do
      local equal = utils.compare(v, rhs[k])
      if not equal then
        return false
      end
    end
  else
    return rhs == lhs
  end
  return true
end
utils.type = function(obj)
  return is_object(obj) and obj:type() or type(obj)
end
utils.flatten = function(t)
  local flat = { }
  for _index_0 = 1, #t do
    local value = t[_index_0]
    if type(value) == "table" then
      local _list_0 = utils.flatten(value)
      for _index_1 = 1, #_list_0 do
        local value = _list_0[_index_1]
        table.insert(flat, value)
      end
    else
      table.insert(flat, value)
    end
  end
  return flat
end
utils.filter = function(func, table)
  return fun.totable(fun.filter(func, table))
end
utils.map = function(func, table)
  return fun.totable(fun.map(func, table))
end
utils.reduce = fun.reduce
utils.dump = function(o)
  if type(o) == 'table' then
    local s = '{'
    for k, v in pairs(o) do
      s = s .. ' ' .. k .. ': ' .. utils.dump(v)
    end
    return s .. ' } '
  else
    return tostring(o)
  end
end
utils.totable = function(...)
  local pats = ...
  if type(pats) == "table" then
    pats = ...
  else
    pats = {
      ...
    }
  end
  return pats
end
utils.zipWith = function(f, xs, ys)
  local _accum_0 = { }
  local _len_0 = 1
  for _, x, y in fun.zip(xs, ys) do
    _accum_0[_len_0] = f(x, y)
    _len_0 = _len_0 + 1
  end
  return _accum_0
end
utils.concat = function(a, b)
  return fun.totable(fun.chain(a, b))
end
utils.splitAt = function(index, value)
  return {
    (function()
      local _accum_0 = { }
      local _len_0 = 1
      local _max_0 = index
      for _index_0 = 1, _max_0 < 0 and #value + _max_0 or _max_0 do
        local v = value[_index_0]
        _accum_0[_len_0] = v
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)(),
    (function()
      local _accum_0 = { }
      local _len_0 = 1
      for _index_0 = index + 1, #value do
        local v = value[_index_0]
        _accum_0[_len_0] = v
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)()
  }
end
utils.rotate = function(arr, step)
  local a, b
  do
    local _obj_0 = utils.splitAt(step, arr)
    a, b = _obj_0[1], _obj_0[2]
  end
  return utils.concat(b, a)
end
utils.id = function(x)
  return x
end
utils.pipe = function(...)
  local funcs = {
    ...
  }
  return fun.reduce((function(f, g)
    return function(...)
      return f(g(...))
    end
  end), utils.id, funcs)
end
utils.curry = function(func, num_args)
  num_args = num_args or 2
  if num_args <= 1 then
    return func
  end
  local curry_h
  curry_h = function(argtrace, n)
    if 0 == n then
      return func(utils.reverse(argtrace()))
    else
      return function(onearg)
        return curry_h((function()
          return onearg, argtrace()
        end), n - 1)
      end
    end
  end
  return curry_h(function() end, num_args)
end
utils.reverse = function(...)
  local reverse_h
  reverse_h = function(acc, v, ...)
    if 0 == select("#", ...) then
      return v, acc()
    else
      return reverse_h((function()
        return v, acc()
      end), ...)
    end
  end
  return reverse_h(function() end, ...)
end
local xorwise
xorwise = function(x)
  local a = bit.bxor(bit.lshift(x, 13), x)
  local b = bit.bxor(bit.rshift(a, 17), a)
  return bit.bxor(bit.lshift(b, 5), b)
end
local _frac
_frac = function(x)
  return (x - x:floor()):asFloat()
end
local timeToIntSeed
timeToIntSeed = function(x)
  return xorwise(floor((_frac(x / 300) * 536870912)))
end
local intSeedToRand
intSeedToRand = function(x)
  return (x % 536870912) / 536870912
end
utils.timeToRand = function(x)
  return abs(intSeedToRand(timeToIntSeed(x)))
end
utils.union = function(a, b)
  local new_map = { }
  for i, v in pairs(b) do
    new_map[i] = v
  end
  for i, v in pairs(a) do
    new_map[i] = v
  end
  return new_map
end
local _string_lambda
_string_lambda = function(f)
  if type(f) == 'function' then
    return f
  end
  if (f:find('^|')) or (f:find('_')) then
    local args, body = f:match('|([^|]*)|(.+)')
    if f:find('_') then
      args = '_'
      body = f
    else
      if not args then
        return error('bad string lambda')
      end
    end
    local fstr = 'return function(' .. args .. ') return ' .. body .. ' end'
    local fn, err = loadstring(fstr)
    if not fn then
      return error(err)
    end
    fn = fn()
    return fn
  else
    return error('not a string lambda')
  end
end
utils.memoize = function(func)
  local cache = { }
  return function(k)
    local res = cache[k]
    if res == nil then
      res = func(k)
      cache[k] = res
    end
    return res
  end
end
utils.string_lambda = utils.memoize(_string_lambda)
return utils
