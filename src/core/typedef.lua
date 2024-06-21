local lpeg = require "lpeg"
local P, S, V, R, C, Ct = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.C, lpeg.Ct

local tremove = table.remove
local tconcat = table.concat

local function pId(...)
   return { tconcat { ... } }
end

local function pComp(const, tvar)
   return { constructor = const[1], tvar[1] }
end

local function pDef(...)
   local args = { ... }
   local name
   if args[1].isname then
      name = tremove(args, 1)[1]
   end
   local ret = tremove(args, #args)
   return { ret = ret, name = name, unpack(args) }
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
local name = V "name"
local ws = S " \n\r\t" ^ 0
local id = ws * ((char ^ 1) / pId) * ws

local rules = {
   [1] = "typedef",
   name = id * ws * P "::" * ws / function(a)
      a.isname = true
      return a
   end,
   typedef = name ^ -1 * (elem * ws * P "->" * ws) ^ 1 * elem / pDef,
   elem = comp_type + id + fdef + tab,
   fdef = P "(" * ws * typedef * ws * P ")",
   tab = P "[" * ws * elem * ws * P "]" / pTab,
   comp_type = id * ws * id / pComp,
}

local grammar = Ct(C(rules))

local TDef = function(a)
   local tdef = grammar:match(a)[2]
   tdef.source = a
   setmetatable(tdef, {
      __tostring = function(self)
         return self.source
      end,
   })
   return tdef, tdef.name
end

return TDef
