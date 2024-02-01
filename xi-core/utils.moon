require "moon.all"
fun = require "fun"
-- bit = require "xi.bitop" --compatability for 5.3 5.4??
bit = require "bitop.funcs" --compatability for 5.3 5.4??

utils = {}

tsize = (t) ->
  size = 0
  for _ in pairs(t)
    size = size + 1
  size

utils.compare = (rhs, lhs) ->
  if type(lhs) != type(rhs)
    return false
  if type(lhs) == "table"
    if tsize(lhs) != tsize(rhs)
      return false
    for k, v in pairs(lhs)
      equal = utils.compare(v, rhs[k])
      if not equal
        return false
  else
    return rhs == lhs
  return true

utils.type = (obj) -> is_object(obj) and obj.__name or type(obj)

utils.flatten = (t) ->
  flat = {}
  for value in *t
    if type(value) == "table"
      for value in *utils.flatten(value)
        table.insert flat, value
    else
        table.insert flat, value
  flat

utils.filter = (func, table) -> fun.totable fun.filter func, table

utils.map = (func, table) -> fun.totable fun.map func, table

utils.reduce = fun.reduce

utils.dump = (o) ->
  if type(o) == 'table'
    -- if tsize(o) then return "{}"
    s = '{'
    for k, v in pairs o
      -- if type(k) ~= 'number' then k = '"' .. k .. '"'
      s = s .. ' ' .. k .. ': ' .. utils.dump(v)
    return s .. ' } '
  else
    return tostring(o)

utils.totable = (...) ->
  pats = ...
  if type(pats) == "table"
    pats = ...
  else
    pats = { ... }
  return pats

utils.zipWith = (f, xs, ys) -> [ f(x,y) for _, x, y in fun.zip(xs, ys) ]

utils.concat = (a, b) -> fun.totable fun.chain a, b

utils.splitAt = (index, value) -> {[v for v in *value[1,index]], [v for v in *value[index + 1,]]}

utils.rotate = (arr, step) ->
  { a, b } = utils.splitAt step, arr
  utils.concat b, a

utils.id = (x) -> x

utils.pipe = (...) ->
  funcs = { ... }
  reduce ((f, g) -> (...) -> f(g(...))), id, funcs

utils.curry = (func, num_args) ->
  num_args = num_args or 2

  if num_args <= 1
    return func

  curry_h = (argtrace, n) ->
    if 0 == n
      return func utils.reverse argtrace!
    else
      return (onearg) -> curry_h((-> onearg, argtrace!), n - 1)
  return curry_h(->, num_args)

utils.reverse = (...) ->
  reverse_h = (acc, v, ...) ->
    if 0 == select("#", ...) then
      return v, acc()
    else
      return reverse_h (-> v, acc()), ...
  return reverse_h(->, ...)

-- random generator
xorwise = (x) ->
  a = bit.bxor(bit.lshift(x,13), x)
  b = bit.bxor(bit.rshift(a,17), a)
  bit.bxor(bit.lshift(b,5), b)

_frac = (x) -> (x - x\floor!)\asFloat!

timeToIntSeed = (x) ->
  xorwise math.floor (_frac(x / 300) * 536870912)

-- output a bit differnet than strudel??
intSeedToRand = (x) -> (x % 536870912) / 536870912

utils.timeToRand = (x) -> math.abs intSeedToRand timeToIntSeed x

utils.union = (a, b) ->
  new_map = {}
  for i, v in pairs(b)
    new_map[i] = v
  for i, v in pairs(a)
    new_map[i] = v
  return new_map

return utils
