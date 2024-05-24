local flatten, zipWith, concat, splitAt, rotate
do
   local _obj_0 = require("modal.utils")
   flatten, zipWith, concat, splitAt, rotate =
      _obj_0.flatten, _obj_0.zipWith, _obj_0.concat, _obj_0.splitAt, _obj_0.rotate
end

local function left(n, m)
   local ons, offs = n[1], n[2]
   local xs, ys = m[1], m[2]
   local _xs, __xs = unpack(splitAt(offs, xs))
   return { offs, ons - offs }, { zipWith(concat, _xs, ys), __xs }
end

local function right(n, m)
   local ons, offs = n[1], n[2]
   local xs, ys = m[1], m[2]
   local _ys, __ys = unpack(splitAt(ons, ys))
   return { ons, offs - ons }, { zipWith(concat, xs, _ys), __ys }
end

local function _bjork(n, m)
   local ons, offs = n[1], n[2]
   if math.min(ons, offs) <= 1 then
      return { n, m }
   else
      if ons > offs then
         return _bjork(left(n, m))
      else
         return _bjork(right(n, m))
      end
   end
end

local function bjork(ons, steps, offset)
   offset = offset and offset or 0
   local offs = steps - ons
   local x, y = {}, {}
   for i = 1, ons do
      x[i] = { true }
   end
   for i = 1, offs do
      y[i] = { false }
   end
   local result = _bjork({ ons, offs }, { x, y })
   result = concat(flatten(result[2][1]), flatten(result[2][2]))
   return rotate(result, offset)
end
-- require("moon.all")
-- p(bjork(3, 8))
return { bjork = bjork }
