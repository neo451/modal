local M = { }
local MOD = 2 ^ 32
local MODM = MOD - 1
local floor = math.floor
local memoize
memoize = function(f)
  local mt = { }
  local t = setmetatable({ }, mt)
  mt.__index = function(self, k)
    local v = f(k)
    self.k = v
    return v
  end
  return t
end
local make_bitop_uncached
make_bitop_uncached = function(t, m)
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
local make_bitop
make_bitop = function(t)
  local op1 = make_bitop_uncached(t, 2 ^ 1)
  local op2 = memoize(function(a)
    return memoize(function(b)
      return op1(a, b)
    end)
  end)
  return make_bitop_uncached(op2, 2 ^ (t.n or 1))
end
M.tobit = function(x)
  return x % 2 ^ 32
end
M.bxor = make_bitop({
  [0] = {
    [0] = 0,
    [1] = 1
  },
  [1] = {
    [0] = 1,
    [1] = 0
  },
  n = 4
})
M.bnot = function(a)
  return MODM - a
end
M.band = function(a, b)
  return ((a + b) - M.bxor(a, b)) / 2
end
M.bor = function(a, b)
  return MODM - M.band(MODM - a, MODM - b)
end
M.rshift = function(a, disp)
  if disp < 0 then
    return M.lshift(a, -disp)
  end
  return floor(a % 2 ^ 32 / 2 ^ disp)
end
M.lshift = function(a, disp)
  if disp < 0 then
    return M.rshift(a, -disp)
  end
  return (a * 2 ^ disp) % 2 ^ 32
end
return M
