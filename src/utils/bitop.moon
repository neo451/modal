-- ported from bitop-lua, droped for dependency issues

M = {}

MOD = 2 ^ 32
MODM = MOD - 1

memoize = (f) ->
  mt = {}
  t = setmetatable {}, mt

  mt.__index = (k) =>
    v = f(k)
    @k = v
    v
  return t

make_bitop_uncached = (t, m) ->
  bitop = (a, b) ->
    res, p = 0, 1
    while a != 0 and b != 0
      am, bm = a % m, b % m
      res = res + t[am][bm] * p
      a = (a - am) / m
      b = (b - bm) / m
      p = p * m
    res = res + (a + b) * p
    res
  return bitop

make_bitop = (t) ->
  op1 = make_bitop_uncached t, 2 ^ 1
  op2 = memoize( (a) ->
    return memoize( (b) ->
      return op1 a, b ) )
  return make_bitop_uncached op2, 2 ^ (t.n or 1)

M.tobit = (x) -> x % 2 ^ 32

M.bxor = make_bitop { [0]: { [0]: 0, [1]: 1 }, [1]: { [0]: 1, [1]: 0 }, n: 4 }

M.bnot = (a) -> MODM - a

M.band = (a, b) -> ((a + b) - M.bxor(a, b)) / 2

M.bor = (a, b) -> MODM - M.band(MODM - a, MODM - b)

M.rshift = (a, disp) ->
  if disp < 0
    return M.lshift(a, -disp)
  return math.floor(a % 2 ^ 32 / 2 ^ disp)

M.lshift = (a, disp) ->
  if disp < 0
    return M.rshift(a, -disp)
  return (a * 2 ^ disp) % 2 ^ 32

return M
