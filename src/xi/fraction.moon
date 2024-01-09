import reduce, type from require 'xi.utils'

decimalToFraction = (x0, err) ->
	err = err or 0.0000000001
	num, den
	g = math.abs x0 -- or x0:abs()
	sign = x0 / g
	a, b, c, d = 0, 1, 1, 0
	s
	iter = 0
	while iter < 1000000
		s = math.floor g
		num = a + s * c
		den = b + s * d
		a, b, c, d = c, d, num, den
		g = 1.0 / (g - s)
		iter = iter + 1
		if err > math.abs sign * num / den - x0
			return sign * num, den

	error "Fraction: failed to find a fraction for " .. x0
	0, 1

gcd = (a, b) ->
	(b == 0) and a or gcd(b, a % b)

lcm = (a, b) ->
	(a == 0 or b == 0) and 0 or math.abs(a * b) / gcd(a, b)

class Fraction
  new: (n = 0, d = 1, normalize = true) =>
    if n % 1 ~= 0
      n, d = decimalToFraction n

    if d == 0
      error("Fraction: divide by zero")

    if normalize and (n ~= 0)
      g = math.floor gcd(n, d)
      n = math.floor n / g
      d = math.floor d / g

    @numerator = n
    @denominator = d

  type: => 'fraction'

  __add: (f2) =>
    if type(f2) == "number"
      f2 = Fraction(f2)

    na = @numerator
    nb = f2.numerator
    da = @denominator
    db = f2.denominator
    g = gcd(da, db)

    if g == 1
      Fraction(na * db + da * nb, da * db, false)

    s = math.floor(da / g)
    t = na * math.floor(db / g) + nb * s
    g2 = gcd(t, g)
    if g2 == 1
      Fraction(t, s * db, false)

    Fraction(math.floor(t / g2), s * math.floor(db / g2), false)

  __sub: (f2) =>
    if type(f2) == "number"
      f2 = Fraction(f2)

    na = @numerator
    nb = f2.numerator
    da = @denominator
    db = f2.denominator
    g = gcd(da, db)

    if g == 1
      Fraction(na * db - da * nb, da * db, false)
    s = math.floor(da / g)
    t = na * math.floor(db / g) - nb * s
    g2 = gcd(t, g)

    if g2 == 1 then
      Fraction(t, s * db, false)

    Fraction(math.floor(t / g2), s * math.floor(db / g2), false)

  __div: (f2) => 
    if type(f2) == "number"
      f2 = Fraction(f2)

    na = @numerator
    nb = f2.numerator
    da = @denominator
    db = f2.denominator

    g1 = gcd(na, nb)
    if g1 > 1
      na = math.floor(na / g1)
      nb = math.floor(nb / g1)

    g2 = gcd(db, da)
    if g2 > 1
      da = math.floor(da / g2)
      db = math.floor(db / g2)

    n = na * db
    d = nb * da
    if d < 0
      n = -n
      d = -d

    Fraction(n, d, false)

  __mul: (f2) =>
    if type(f2) == "number"
      f2 = Fraction(f2)

    na = @numerator
    nb = f2.numerator
    da = @denominator
    db = f2.denominator

    g1 = gcd(na, db)
    if g1 > 1
      na = math.floor(na / g1)
      db = math.floor(db / g1)

    g2 = gcd(nb, da)
    if g2 > 1
      nb = math.floor(nb / g2)
      da = math.floor(da / g2)

    Fraction(na * nb, da * db, false)

  __pow: (f2) =>
    if type(f2) == "number"
      f2 = Fraction(f2)

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
    if type(f2) == "number"
      f2 = Fraction(f2)

    da = @denominator
    db = f2.denominator
    na = @numerator
    nb = f2.numerator

    Fraction (na * db) % (nb * da), da * db

  __unm: => Fraction -@numerator, @denominator, false

  __eq:(rhs) => @numerator / @denominator == rhs.numerator / rhs.denominator

  __lt:(rhs) => @numerator / @denominator < rhs.numerator / rhs.denominator

  __lte:(rhs) => @numerator / @denominator <= rhs.numerator / rhs.denominator

  floor: => math.floor @numerator / @denominator

  -- TODO: need tests
  sam: => Fraction @floor!

  nextSam: => @sam! + 1

  min:(other) =>
    if type(other) == "number"
      other = Fraction(other)

    if @ < other
      return @
    else
      return other

  max:(other) =>
    if type(other) == "number"
      other = Fraction(other)

    if @ > other
      return @
    else
      return other

  -- TODO: need test
  gcd:(other) =>
    gcd_numerator = gcd @numerator, other.numerator
    lcm_denominator = lcm @denominator, other.denominator
    Fraction gcd_numerator, lcm_denominator

  asFloat: => @numerator / @denominator

  __tostring: => string.format "%d/%d", @numerator, @denominator

  show: => @__tostring!

gcd_reduce = (table) ->
  reduce ((acc, value) -> acc\gcd value), table[1], table

return {
  Fraction: Fraction
  gcd: gcd_reduce
}
