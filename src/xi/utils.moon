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

utils.reduce = (func, init, table) -> reduce func, init, table

-- TODO: work on this more
utils.dump = (o) ->
    if utils.type(o) == 'table' then
        s = '{\n'
        for k, v in pairs o
            if utils.type(k) ~= 'number' then k = '"' .. k .. '"'
            s = s .. '  [' .. k .. '] = ' .. utils.dump(v) .. ',\n'
        return s .. '} '
    else
        return tostring(o)

utils.id = (x) -> x

return utils
