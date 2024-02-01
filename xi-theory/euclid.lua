local left
left = function(n, m)
  local ons, offs
  ons, offs = n[1], n[2]
  local xs, ys
  xs, ys = m[1], m[2]
  local _xs, __xs
  do
    local _obj_0 = splitAt(offs, xs)
    _xs, __xs = _obj_0[1], _obj_0[2]
  end
  return {
    offs,
    ons - offs
  }, {
    zipWith((function(a, b)
      return concat(a, b)
    end), _xs, ys),
    __xs
  }
end
local right
right = function(n, m)
  local ons, offs
  ons, offs = n[1], n[2]
  local xs, ys
  xs, ys = m[1], m[2]
  local _ys, __ys
  do
    local _obj_0 = splitAt(ons, ys)
    _ys, __ys = _obj_0[1], _obj_0[2]
  end
  return {
    ons,
    offs - ons
  }, {
    zipWith((function(a, b)
      return concat(a, b)
    end), xs, _ys),
    __ys
  }
end
local _bjork
_bjork = function(n, m)
  local ons, offs
  ons, offs = n[1], n[2]
  if math.min(ons, offs) <= 1 then
    return {
      n,
      m
    }
  else
    if ons > offs then
      return _bjork(left(n, m))
    else
      return _bjork(right(n, m))
    end
  end
end
local bjork
bjork = function(ons, steps, offset)
  if offset == nil then
    offset = 0
  end
  local offs = steps - ons
  local x
  do
    local _accum_0 = { }
    local _len_0 = 1
    for i = 1, ons do
      _accum_0[_len_0] = {
        true
      }
      _len_0 = _len_0 + 1
    end
    x = _accum_0
  end
  local y
  do
    local _accum_0 = { }
    local _len_0 = 1
    for i = 1, offs do
      _accum_0[_len_0] = {
        false
      }
      _len_0 = _len_0 + 1
    end
    y = _accum_0
  end
  local result = _bjork({
    ons,
    offs
  }, {
    x,
    y
  })
  result = concat(flatten(result[2][1]), flatten(result[2][2]))
  return rotate(result, offset)
end
return {
  bjork = bjork
}
