require("moon.all")

local pp = require("metalua.pprint").print
local lpeg = require("lpeg")

local V, R, P, S = lpeg.V, lpeg.R, lpeg.P, lpeg.S
local C, Ct = lpeg.C, lpeg.Ct

local root = V("root")
local symb = V("symb")
local str = V("str")
local call = V("call")
local def = V("def")
local unquote = V("unquote")
local dollar = V("dollar")
local list = V("list")
local op = V("op")
local item = V("item")
local mini = V("mini")
local pr = V("pr")
local pm = V("pm")
local spm = V("spm")

local slice_with_ops = V("slice_with_ops")
local mop = V("mop")
local fast = V("fast")
local slow = V("slow")

local function Id(a)
   return { tag = "Id", a }
end

local function unquote(a)
   for i, v in pairs(a) do
      if v.tag == "Quote" then
         if v[1].tag == "Id" then
            v[1].tag = "String"
         elseif v[1].tag == "String" then
            v[1].tag = "Id"
         end
         a[i] = v[1]
      end
      if v.tag == "Id" then
         v.tag = "String"
         a[i] = v
      end
   end
   return a
end

local function ppr(...)
   local res = unquote({ ... })
   return { tag = "Call", Id("fastcat"), unpack(res) }
end

local function pspm(...)
   local res = unquote({ ... })
   return { tag = "Call", Id("slowcat"), unpack(res) }
end

local function psymb(s)
   return { tag = "Id", s }
end

local function pnum(n)
   return { tag = "Number", n }
end

local opsymb = {
   ["+"] = "add",
   ["-"] = "sub",
   ["*"] = "mul",
   ["/"] = "div",
   ["^"] = "pow",
   ["%"] = "mod",
   -- TODO: tidal ops!!
}

local function is_op(a)
   return opsymb[a]
end

local function pdot(...)
   return { ... }
end

local function plist(args)
   if #args == 3 then
      if is_op(args[2][1]) then
         local opname = opsymb[args[2][1]]
         table.remove(args, 2)
         return { tag = "Op", opname, unpack(args) }
      elseif is_op(args[1][1]) then
         local opname = opsymb[args[1][1]]
         table.remove(args, 1)
         return { tag = "Op", opname, unpack(args) }
      elseif args[1].tag == "Id" then
         local fname = args[1]
         table.remove(args, 1)
         return { tag = "Call", fname, unpack(args) }
      end
   end
end

local function punquote(a)
   return { tag = "Quote", a }
end

local function pslice(...)
   local args = { ... }
   local acc = args[1]
   table.remove(args, 1)
   for _, v in pairs(args) do
      acc = { tag = "Call", Id(v.tag), v[1], acc }
   end
   return acc
end

local ws = S(" \n\r\t") ^ 0
local minus = P("-")
local plus = P("+")
local zero = P("0")
local digit = R("09")
local decimal_point = P(".")
local digit1_9 = R("19")
local e = S("eE")
local int = zero + (digit1_9 * digit ^ 0)
local intneg = minus ^ -1 * int
local exp = e * (minus + plus) ^ -1 * digit ^ 1
local frac = decimal_point * digit ^ 1
local num = (minus ^ -1 * int * frac ^ -1 * exp ^ -1) / pnum
local slice = V("slice")
local sequence = V("sequence")

local pseq = function(...)
   return { ... }
end

local rules = {
   [1] = root,
   op = S("-+/&%^"),
   root = ws * (dollar + call + symb + list + mini) * ws,
   symb = ws * ((R("AZ", "az", "09") + op) ^ 1 / psymb) * ws,

   unquote = P("`") * (num + symb + list) / punquote,
   item = num + symb + list + mini + unquote + dollar,

   sequence = (slice_with_ops ^ 1) / pseq,
   slice = num + symb + mini + unquote,
   call = (ws * item * ws) ^ 1 / pdot,
   mini = pr + spm,

   pr = P("[") * ws * sequence * ws * P("]") / ppr,
   spm = P("<") * ws * sequence * ws * P(">") / pspm,

   list = P("(") * call * P(")") / plist,
   dollar = P("$") * ws * call / plist,

   slice_with_ops = (item * mop ^ 0) / pslice,
   mop = fast + slow,
   fast = P("*") * item / function(a)
      return { tag = "fast", a }
   end,
   slow = P("/") * item / function(a)
      return { tag = "slow", a }
   end,
}

local pat = Ct(C(rules))

local function read(src)
   return pat:match(src)[2]
end

local mlc = require("metalua.compiler").new()

local function eval(src)
   local ast = read(src)
   local lua_src = mlc:ast_to_src(ast)
   return loadstring("require'modal'(); a = fast(2, reify(1)); return " .. lua_src)()
end

pp(eval([[ [a b] ]]))
return eval
