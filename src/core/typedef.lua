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

local function show_sig(t)
   local function format(a)
      if type(a[1]) == "table" then
         return ("(%s)"):format(show_sig(a))
      elseif a.constructor then
         return ("%s %s"):format(a.constructor, a[1])
      elseif type(a) == "string" then
         return a
      end
   end
   local s = {}
   for i = 1, #t do
      s[#s + 1] = format(t[i])
      s[#s + 1] = " -> "
   end
   s[#s + 1] = format(t.ret)
   return tconcat(s)
end

local TDef = function(a)
   local tdef = grammar:match(a)[2]
   return setmetatable(tdef, {
      __tostring = function(self)
         return show_sig(self)
      end,
   }),
      tdef.name
end

return TDef
