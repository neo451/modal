import reduce from require 'xi.fun'
import type from require "xi.utils"
local *

abs = math.abs
floor = math.floor

-- decimaltofraction1 = (x, N) ->
--     N = N or 1000000
--     a, b = 0, 1
--     c, d = 1, 1
--     while b <= N and d <= N
--         mediant = (a+c)/(b+d)
--         if x == mediant
--             if b + d <= N
--                 return a + c, b + d
--             elseif d > b
--                 return c, d
--             else
--                 return a, b
--         elseif x > mediant
--             a, b = a + c, b + d
--         else
--             c, d = a + c, b + d
--
--     if b > N
--         return c, d
--     else
--         return a, b

decimaltofraction = (x0, err) ->
  err = err or 0.0000000001
  local num, den
  g = abs x0
  sign = x0 / g
  a, b, c, d = 0, 1, 1, 0
  local s
  iter = 0
  while iter < 1000000
    s = floor g
    num = a + s * c
    den = b + s * d
    a, b, c, d = c, d, num, den
    g = 1.0 / (g - s)
    iter = iter + 1

    if err > abs sign * num / den - x0
      return sign * num, den

  error "Fraction: failed to find a fraction for " .. x0
  return 0, 1

gcd = (a, b) -> (b == 0) and a or gcd(b, a % b)

lcm = (a, b) -> (a == 0 or b == 0) and 0 or abs(a * b) / gcd(a, b)

class Fraction
  new: (n = 0, d = 1, normalize = true) =>
    if n % 1 ~= 0
      n, d = decimaltofraction n

    if d == 0
      error("Fraction: divide by zero")

    if normalize and (n ~= 0)
      g = floor gcd(n, d)
      n = floor n / g
      d = floor d / g

    @numerator = n
    @denominator = d

  type: -> "fraction"

  __add: (f2) =>
    f2 = tofrac f2
    na = @numerator
    nb = f2.numerator
    da = @denominator
    db = f2.denominator
    g = gcd(da, db)

    if g == 1
      Fraction(na * db + da * nb, da * db, false)

    s = floor(da / g)
    t = na * floor(db / g) + nb * s
    g2 = gcd(t, g)
    if g2 == 1
      Fraction(t, s * db, false)

    Fraction(floor(t / g2), s * floor(db / g2), false)

  __sub: (f2) =>
    f2 = tofrac f2
    na = @numerator
    nb = f2.numerator
    da = @denominator
    db = f2.denominator
    g = gcd(da, db)

    if g == 1
      Fraction(na * db - da * nb, da * db, false)
    s = floor(da / g)
    t = na * floor(db / g) - nb * s
    g2 = gcd(t, g)

    if g2 == 1 then
      Fraction(t, s * db, false)

    Fraction(floor(t / g2), s * floor(db / g2), false)

  __div: (f2) =>
    f2 = tofrac f2
    na = @numerator
    nb = f2.numerator
    da = @denominator
    db = f2.denominator

    g1 = gcd(na, nb)
    if g1 > 1
      na = floor(na / g1)
      nb = floor(nb / g1)

    g2 = gcd(db, da)
    if g2 > 1
      da = floor(da / g2)
      db = floor(db / g2)

    n = na * db
    d = nb * da
    if d < 0
      n = -n
      d = -d

    Fraction(n, d, false)

  __mul: (f2) =>
    f2 = tofrac f2
    na = @numerator
    nb = f2.numerator
    da = @denominator
    db = f2.denominator

    g1 = gcd(na, db)
    if g1 > 1
      na = floor(na / g1)
      db = floor(db / g1)

    g2 = gcd(nb, da)
    if g2 > 1
      nb = floor(nb / g2)
      da = floor(da / g2)

    Fraction(na * nb, da * db, false)

  __pow: (f2) =>
    f2 = tofrac f2
    if f2.denominator == 1
      power = f2.numerator
      if power >= 0
        Fraction(@numerator ^ power, @denominator ^ power, false)
      elseif @numerator >= 0
        Fraction(@denominator ^ -power, @numerator ^ -power, false)
      else
        Fraction((-@numerator) ^ -power, (-@denominator) ^ -power, false)
    else
      (@numerator / @denominator) ^ (f2.numerator / f2.denominator)

  __mod: (f2) =>
    f2 = tofrac f2
    da = @denominator
    db = f2.denominator
    na = @numerator
    nb = f2.numerator

    Fraction (na * db) % (nb * da), da * db

  __unm: => Fraction -@numerator, @denominator, false

  __eq:(rhs) => @numerator / @denominator == rhs.numerator / rhs.denominator

  __lt:(rhs) => @numerator / @denominator < rhs.numerator / rhs.denominator

  __lte:(rhs) => @numerator / @denominator <= rhs.numerator / rhs.denominator

  floor: => floor @numerator / @denominator

  sam: => Fraction @floor!

  nextSam: => @sam! + 1

  min:(other) =>
    other = tofrac other
    if @ < other
      return @
    else
      return other

  max:(other) =>
    other = tofrac other
    if @ > other
      return @
    else
      return other

  gcd:(other) =>
    other = tofrac other
    gcd_numerator = gcd @numerator, other.numerator
    lcm_denominator = lcm @denominator, other.denominator
    Fraction gcd_numerator, lcm_denominator

  asFloat: => @numerator / @denominator

  __tostring: =>
    string.format "%d/%d", @numerator, @denominator
    -- str = string.format "%d/%d", @numerator, @denominator
    -- color("{red}str")

  show: => @__tostring!

gcd_reduce = (table) ->
  reduce ((acc, value) -> acc\gcd value), table[1], table

tofrac = (x) ->
  if type(x) == "number"
    return Fraction(x)
  else
    return x

return { :Fraction, :gcd_reduce, :tofrac, :tofloat }
