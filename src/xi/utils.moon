require "moon.all"
utils = {}

TableSize = (t) ->
  size = 0
  for _ in pairs(t) 
    size = size + 1
  size

utils.compare = (rhs, lhs) ->
  if type(lhs) != type(rhs)
    return false
  if type(lhs) == "table"
    if TableSize(lhs) != TableSize(rhs)
      return false
    for k, v in pairs(lhs)
      equal = utils.compare(v, rhs[k])
      if not equal
        return false
  else
    return rhs == lhs
  return true

utils.type = (obj) ->
  if is_object(obj)
    return obj\type!
  return type(obj)

utils.map = (items, func) -> [func(item) for item in * items]

utils.filter = (items, func) -> [item for item in *items when func(item)]

utils.fold = fold

-- TODO: flatten

return utils
