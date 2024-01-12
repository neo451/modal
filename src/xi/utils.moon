require "moon.all"
(require "fun")!

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

utils.type = (obj) -> is_object(obj) and obj\type! or type(obj)

utils.flatten = (t) ->
  flat = {}
  for value in *t
    if type(value) == "table"
      for value in *utils.flatten(value)
        table.insert flat, value
    else
        table.insert flat, value
  flat

utils.filter = (func, table) -> totable filter func, table

utils.map = (func, table) -> totable map func, table

-- TODO: work on this more
utils.dump = (o) ->
    if type(o) == 'table' then
        s = '{\n'
        for k, v in pairs o
            if type(k) ~= 'number' then k = '"' .. k .. '"'
            s = s .. '  [' .. k .. '] = ' .. utils.dump(v) .. ',\n'
        return s .. '} '
    else
        return tostring(o)

utils.id = (x) -> x

utils.pipe = (...) ->
  funcs = { ... }
  reduce ((f, g) -> (...) -> f(g(...))), id, funcs

-- test
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

return utils
