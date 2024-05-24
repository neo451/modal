local reduce
reduce = require("modal.fun").reduce
local T
T = require("modal.utils").type
local abs, floor, decimaltofraction, gcd, lcm, Fraction, gcd_reduce, tofrac, tofloat
abs = math.abs
floor = math.floor
decimaltofraction = function(x0, err)
   err = err or 0.0000000001
   local num, den
   local g = abs(x0)
   local sign = x0 / g
   local a, b, c, d = 0, 1, 1, 0
   local s
   local iter = 0
   while iter < 1000000 do
      s = floor(g)
      num = a + s * c
      den = b + s * d
      a, b, c, d = c, d, num, den
      g = 1.0 / (g - s)
      iter = iter + 1
      if err > abs(sign * num / den - x0) then
         return sign * num, den
      end
   end
   error("Fraction: failed to find a fraction for " .. x0)
   return 0, 1
end
gcd = function(a, b)
   return (b == 0) and a or gcd(b, a % b)
end
lcm = function(a, b)
   return (a == 0 or b == 0) and 0 or abs(a * b) / gcd(a, b)
end
do
   local _class_0
   local _base_0 = {
      type = function()
         return "fraction"
      end,
      __add = function(self, f2)
         f2 = tofrac(f2)
         local na = self.numerator
         local nb = f2.numerator
         local da = self.denominator
         local db = f2.denominator
         local g = gcd(da, db)
         if g == 1 then
            Fraction(na * db + da * nb, da * db, false)
         end
         local s = floor(da / g)
         local t = na * floor(db / g) + nb * s
         local g2 = gcd(t, g)
         if g2 == 1 then
            Fraction(t, s * db, false)
         end
         return Fraction(floor(t / g2), s * floor(db / g2), false)
      end,
      __sub = function(self, f2)
         f2 = tofrac(f2)
         local na = self.numerator
         local nb = f2.numerator
         local da = self.denominator
         local db = f2.denominator
         local g = gcd(da, db)
         if g == 1 then
            Fraction(na * db - da * nb, da * db, false)
         end
         local s = floor(da / g)
         local t = na * floor(db / g) - nb * s
         local g2 = gcd(t, g)
         if g2 == 1 then
            Fraction(t, s * db, false)
         end
         return Fraction(floor(t / g2), s * floor(db / g2), false)
      end,
      __div = function(self, f2)
         f2 = tofrac(f2)
         local na = self.numerator
         local nb = f2.numerator
         local da = self.denominator
         local db = f2.denominator
         local g1 = gcd(na, nb)
         if g1 > 1 then
            na = floor(na / g1)
            nb = floor(nb / g1)
         end
         local g2 = gcd(db, da)
         if g2 > 1 then
            da = floor(da / g2)
            db = floor(db / g2)
         end
         local n = na * db
         local d = nb * da
         if d < 0 then
            n = -n
            d = -d
         end
         return Fraction(n, d, false)
      end,
      __mul = function(self, f2)
         f2 = tofrac(f2)
         local na = self.numerator
         local nb = f2.numerator
         local da = self.denominator
         local db = f2.denominator
         local g1 = gcd(na, db)
         if g1 > 1 then
            na = floor(na / g1)
            db = floor(db / g1)
         end
         local g2 = gcd(nb, da)
         if g2 > 1 then
            nb = floor(nb / g2)
            da = floor(da / g2)
         end
         return Fraction(na * nb, da * db, false)
      end,
      __pow = function(self, f2)
         f2 = tofrac(f2)
         if f2.denominator == 1 then
            local power = f2.numerator
            if power >= 0 then
               return Fraction(self.numerator ^ power, self.denominator ^ power, false)
            elseif self.numerator >= 0 then
               return Fraction(self.denominator ^ -power, self.numerator ^ -power, false)
            else
               return Fraction((-self.numerator) ^ -power, (-self.denominator) ^ -power, false)
            end
         else
            return (self.numerator / self.denominator) ^ (f2.numerator / f2.denominator)
         end
      end,
      __mod = function(self, f2)
         f2 = tofrac(f2)
         local da = self.denominator
         local db = f2.denominator
         local na = self.numerator
         local nb = f2.numerator
         return Fraction((na * db) % (nb * da), da * db)
      end,
      __unm = function(self)
         return Fraction(-self.numerator, self.denominator, false)
      end,
      __eq = function(self, rhs)
         return self.numerator / self.denominator == rhs.numerator / rhs.denominator
      end,
      __lt = function(self, rhs)
         return self.numerator / self.denominator < rhs.numerator / rhs.denominator
      end,
      __lte = function(self, rhs)
         return self.numerator / self.denominator <= rhs.numerator / rhs.denominator
      end,
      floor = function(self)
         return floor(self.numerator / self.denominator)
      end,
      sam = function(self)
         return Fraction(self:floor())
      end,
      nextSam = function(self)
         return self:sam() + 1
      end,
      min = function(self, other)
         other = tofrac(other)
         if self < other then
            return self
         else
            return other
         end
      end,
      max = function(self, other)
         other = tofrac(other)
         if self > other then
            return self
         else
            return other
         end
      end,
      gcd = function(self, other)
         other = tofrac(other)
         local gcd_numerator = gcd(self.numerator, other.numerator)
         local lcm_denominator = lcm(self.denominator, other.denominator)
         return Fraction(gcd_numerator, lcm_denominator)
      end,
      asFloat = function(self)
         return self.numerator / self.denominator
      end,
      __tostring = function(self)
         return string.format("%d/%d", self.numerator, self.denominator)
      end,
      show = function(self)
         return self:__tostring()
      end,
   }
   _base_0.__index = _base_0
   _class_0 = setmetatable({
      __init = function(self, n, d, normalize)
         if n == nil then
            n = 0
         end
         if d == nil then
            d = 1
         end
         if normalize == nil then
            normalize = true
         end
         if n % 1 ~= 0 then
            n, d = decimaltofraction(n)
         end
         if d == 0 then
            error("Fraction: divide by zero")
         end
         if normalize and (n ~= 0) then
            local g = floor(gcd(n, d))
            n = floor(n / g)
            d = floor(d / g)
         end
         self.numerator = n
         self.denominator = d
      end,
      __base = _base_0,
      __name = "Fraction",
   }, {
      __index = _base_0,
      __call = function(cls, ...)
         local _self_0 = setmetatable({}, _base_0)
         cls.__init(_self_0, ...)
         return _self_0
      end,
   })
   _base_0.__class = _class_0
   Fraction = _class_0
end
gcd_reduce = function(table)
   return reduce(function(acc, value)
      return acc:gcd(value)
   end, table[1], table)
end
tofrac = function(x)
   if T(x) == "number" then
      return Fraction(x)
   else
      return x
   end
end
tofloat = function(x)
   if T(x) == "fraction" then
      return x:asFloat()
   else
      return x
   end
end
return {
   Fraction = Fraction,
   gcd_reduce = gcd_reduce,
   tofrac = tofrac,
   tofloat = tofloat,
}
