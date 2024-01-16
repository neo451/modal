import flatten, zipWith, concat, splitAt, rotate from require "xi.utils"

left = (n, m) ->
  { ons, offs } = n
  { xs, ys } = m
  { _xs, __xs } = splitAt offs, xs
  return { offs, ons - offs }, { zipWith(((a, b) -> concat a, b), _xs, ys), __xs}


right = (n, m) ->
  { ons, offs } = n
  { xs, ys } = m
  { _ys, __ys } = splitAt ons, ys
  return { ons, offs - ons }, { zipWith(((a, b) -> concat a, b), xs, _ys), __ys }


_bjork = (n, m) ->
  { ons, offs } = n
  if math.min(ons, offs) <= 1
    return { n, m }
  else
    if ons > offs
      return _bjork left(n, m)
    else
      return _bjork right(n, m)

bjork = (ons, steps, offset = 0) ->
  offs = steps - ons
  x = [ {1} for i = 1, ons ]
  y = [ {0} for i = 1, offs ]
  result = _bjork { ons, offs }, { x, y }
  result = concat flatten(result[2][1]), flatten(result[2][2])
  rotate result, offset

return { :bjork }
