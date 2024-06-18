local ut = require "modal.utils"
local flatten, zipWith, concat, splitAt, rotate = ut.flatten, ut.zipWith, ut.concat, ut.splitAt, ut.rotate

local min = math.min

local function left(n, m)
   local ons, offs = n[1], n[2]
   local xs, ys = m[1], m[2]
   local _xs, __xs = splitAt(offs, xs)
   return { offs, ons - offs }, { zipWith(concat, _xs, ys), __xs }
end

local function right(n, m)
   local ons, offs = n[1], n[2]
   local xs, ys = m[1], m[2]
   local _ys, __ys = splitAt(ons, ys)
   return { ons, offs - ons }, { zipWith(concat, xs, _ys), __ys }
end

local function _bjork(n, m)
   local ons, offs = n[1], n[2]
   if min(ons, offs) <= 1 then
      return { n, m }
   else
      if ons > offs then
         return _bjork(left(n, m))
      else
         return _bjork(right(n, m))
      end
   end
end

return function(ons, steps, offset)
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
   return rotate(offset, result)
end
