local lpeg = require "lpeg"
local P, S, V, R, C, Ct = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.C, lpeg.Ct
require "moon.all"
local function pId(...)
   return table.concat { ... }
end

local function pComp(...)
   local args = { ... }
   local constructor = table.remove(args, 1)
   args.constructor = constructor
   return args
end

local function pDef(...)
   local args = { ... }
   local ret = table.remove(args, #args)
   args.ret = ret
   -- p(args)
   return args
end
local typedef = V "typedef"
local fdef = V "fdef"
local elem = V "elem"
local comp_type = V "comp_type"
local char = R("AZ", "az")
local ws = S " \n\r\t" ^ 0
local id = ws * ((char ^ 1) / pId) * ws

local rules = {
   "typedef",
   typedef = (elem * ws * P "->" * ws) ^ 1 * elem / pDef,
   elem = comp_type + id + fdef,
   fdef = P "(" * ws * typedef * ws * P ")",
   comp_type = id * ws * id / pComp,
}

local grammar = Ct(C(rules))

local read = function(a)
   return grammar:match(a)[2]
end

-- assert(read "Pattern Time -> (Pattern a  -> Pattern a) -> Pattern a", {
--    {
--       "Time",
--       constructor = "Pattern",
--    },
--    {
--       {
--          "a",
--          constructor = "Pattern",
--       },
--       ret = {
--          "a",
--          constructor = "Pattern",
--       },
--    },
--    ret = {
--       "a",
--       constructor = "Pattern",
--    },
-- })

local TDef = {}

function TDef:new(str)
   local newobj = { T = read(str) }
   self.__index = self
   return setmetatable(newobj, self)
end

local function gen_T(t)
   local function format(a)
      if type(a[1]) == "table" then
         return string.format("(%s)", gen_T(a))
      end
      return string.format("%s %s", a.constructor, a[1])
   end
   local s = ""
   for i = 1, #t do
      s = s .. format(t[i]) .. " -> "
   end
   return s .. format(t.ret)
end

function TDef:__tostring()
   return gen_T(self.T)
end

-- print(TDef:new "Pattern Time -> Pattern a -> Pattern a")
-- print(TDef:new "Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a")
return TDef
