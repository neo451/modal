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

local function show_sig(t)
   local function format(a)
      if type(a[1]) == "table" then
         return string.format("(%s)", show_sig(a))
      elseif a.constructor then
         return string.format("%s %s", a.constructor, a[1])
      elseif type(a) == "string" then
         return a
      end
   end
   local s = ""
   for i = 1, #t do
      s = s .. format(t[i]) .. " -> "
   end
   return s .. format(t.ret)
end

local TDef = function(a)
   return setmetatable(grammar:match(a)[2], {
      __tostring = function(self)
         return show_sig(self)
      end,
   })
end

return TDef
