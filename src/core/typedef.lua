local lpeg = require "lpeg"
local P, S, V, R, C, Ct = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.C, lpeg.Ct

local function pId(...)
   return { table.concat { ... } }
end

local function pComp(const, tvar)
   return { constructor = const[1], tvar[1] }
end

local function pDef(...)
   local args = { ... }
   local ret = table.remove(args, #args)
   return { ret = ret, unpack(args) }
end

local function pTab(a)
   a.istable = true
   return a
end

local typedef = V "typedef"
local fdef = V "fdef"
local tab = V "tab"
local elem = V "elem"
local comp_type = V "comp_type"
local char = R("AZ", "az")
local ws = S " \n\r\t" ^ 0
local id = ws * ((char ^ 1) / pId) * ws

local rules = {
   [1] = "typedef",
   typedef = (elem * ws * P "->" * ws) ^ 1 * elem / pDef,
   elem = comp_type + id + fdef + tab,
   fdef = P "(" * ws * typedef * ws * P ")",
   tab = P "[" * ws * elem * ws * P "]" / pTab,
   comp_type = id * ws * id / pComp,
}

local grammar = Ct(C(rules))

local TDef = function(a)
   return grammar:match(a)[2]
end

return TDef
