local ut = require "ut"
local notation = {}

local lpeg = require "lpeg"
local P, S, V, R, C, Ct = lpeg.P, lpeg.S, lpeg.V, lpeg.R, lpeg.C, lpeg.Ct

local a2s = require "a2s"

local loadstring = ut.loadstring
local setfenv = setfenv or ut.setfenv
local memoize = ut.memoize
local tremove = table.remove
local ipairs = ipairs
local type = type
local filter = ut.filter
local map = ut.map
local reduce = ut.reduce
local unpack = _G.unpack or rawget(table, "unpack")

local sequence = V "sequence"
local slice = V "slice"
local sub_cycle = V "sub_cycle"
local polymeter = V "polymeter"
local slow_sequence = V "slow_sequence"
local polymeter_steps = V "polymeter_steps"
local stack = V "stack"
local mini = V "mini"
local op = V "op"
local fast = V "fast"
local slow = V "slow"
local rand = V "rand"
local replicate = V "replicate"
local degrade = V "degrade"
local weight = V "weight"
local euclid = V "euclid"
local tail = V "tail"
local range = V "range"
local list = V "list"
local dollar = V "dollar"
local tailop = V "tailop"
local expr = V "expr"
local ret = V "ret"
local stat = V "stat"
local choose = V "choose"
local dotStack = V "dotStack"

local function Id(a)
   return { tag = "Id", a }
end

local function Table(a)
   return { tag = "Table", unpack(a) }
end

local function Str(a)
   return { tag = "String", a }
end

local function Num(a)
   return { tag = "Number", a }
end

local function id(x)
   return x
end

local function Call(name, ...)
   return { tag = "Call", Id(name), ... }
end

local seed = -1 -- TODO: use this?
local ws = S " \n\r\t" ^ 0
local comma = ws * P "," * ws
local pipe = ws * P "|" * ws
local dot = ws * P "." * ws

local function pNumber(num)
   return Num(tonumber(num))
end

local bool = { ["t"] = { tag = "True" }, ["f"] = { tag = "False" } }

local function pStep(chars)
   if chars == "~" then
      return Id "silence"
   elseif tonumber(chars) then
      return Num(tonumber(chars))
   elseif bool[chars] then
      return bool[chars]
   elseif chars:sub(0, 1) == "'" then
      return Id(chars:sub(2, #chars))
   end
   return Str(chars)
end

local function rTails(args)
   local f = tremove(args, 1)
   if f.tag == "String" then
      f.tag = "Id"
   end
   local params = filter(function(a)
      return type(a) ~= "function"
   end, args)
   local tails = filter(function(a)
      return type(a) == "function"
   end, args)
   local main = { tag = "Call", f, unpack(params) }
   for i = 1, #tails do
      main = tails[i](main)
   end
   return main
end

local step_char = R("09", "AZ", "az") + S [[~^'._]]
local tidalop = (S "|+-*/^%><" ^ 2 + P "#") / id
local arith = (S "+-*/^%" - P "|") / id
local step = ws * ((step_char ^ 1 - P ".") / pStep) * ws
local minus = P "-"
local plus = P "+"
local zero = P "0"
local digit = R "09"
local decimal_point = P "."
local digit1_9 = R "19"
local e = S "eE"
local int = zero + (digit1_9 * digit ^ 0)
local exp = e * (minus + plus) ^ -1 * digit ^ 1
local frac = decimal_point * digit ^ 1
local number = (minus ^ -1 * int * frac ^ -1 * exp ^ -1) / pNumber

local function pFast(a)
   return function(x)
      return Call("fast", a, x)
   end
end

local function pSlow(a)
   return function(x)
      return Call("slow", a, x)
   end
end

-- local function pRand(a)
--    lower = a[1] or 0
--    return function(x)
--       -- TODO: idea rand run
--       return Num(math.random(lower, x[1]))
--    end
-- end

local function pDegrade(a)
   if a == "?" then
      a = Num(0.5)
   end
   return function(x)
      seed = seed + 1
      return Call("degradeBy", a, x)
   end
end

local function pTail(b)
   return function(a)
      return Call("chain", a, b)
   end
end

local function pEuclid(p, s, r)
   r = r or Num(0)
   return function(x)
      return Call("euclidRot", p, s, r, x)
   end
end

local function pRange(s)
   return function(x)
      x.range = s[1]
      x.reps = nil
      return x
   end
end

local function pWeight(a)
   return function(x)
      x.weight = (x.weight or 1) + (tonumber(a[1]) or 2) - 1
      return x
   end
end

local function pReplicate(a)
   return function(x)
      x.reps = (x.reps or 1) + (tonumber(a[1]) or 2) - 1
      return x
   end
end

local function rReps(ast)
   local res = {}
   for _, node in ipairs(ast) do
      if node.reps then
         local reps = node.reps
         for _ = 1, reps do
            node.reps = nil
            res[#res + 1] = node
         end
      elseif node.range then
         for i = node[1], node.range do
            res[#res + 1] = Num(i)
         end
      else
         res[#res + 1] = node
      end
   end
   return res
end

local function pSlices(sli, ...)
   for _, v in ipairs { ... } do
      sli = v(sli)
   end
   return sli
end

local function addWeight(a, b)
   b = b.weight and b.weight or 1
   return a + b
end

local function rWeight(args)
   local acc = {}
   for _, v in ipairs(args) do
      acc[#acc + 1] = v.weight and Num(v.weight) or Num(1)
      acc[#acc + 1] = v
   end
   return acc
end

local function pSeq(isSlow)
   return function(args)
      local weightSum = reduce(addWeight, 0, args)
      if weightSum > #args then
         return Call(isSlow and "arrange" or "timecat", Table(rWeight(args)))
      else
         if #args == 1 then
            if isSlow then
               return Call("pure", args[1])
            end
            return args[1]
         end
         return Call(isSlow and "slowcat" or "fastcat", Table(args))
      end
   end
end

local function pStack(...)
   local args = map(rReps, { ... })
   return rReps(args), "Stack"
end

local function pChoose(...)
   local args = map(rReps, { ... })
   return rReps(args), "Choose"
end

local function pDotStack(...)
   local args = map(rReps, { ... })
   return rReps(args), "DotStack"
end

local opsymb = {
   ["+"] = "add",
   ["-"] = "sub",
   ["*"] = "mul",
   ["/"] = "div",
   ["^"] = "pow",
   ["%"] = "mod",
   -- ["+"] = { "add", true },
   -- ["-"] = { "sub", true },
   -- ["*"] = { "mul", true },
   -- ["/"] = { "div", true },
   -- ["^"] = { "pow", true },
   -- ["%"] = { "mod", true },
   -- ["."] = { "pipe", false },
}

local function is_op(a)
   return opsymb[a]
end

local function pDollar(...)
   local args = { ... }
   if #args == 1 then
      return args
   end
   return rTails(args)
end

local function pList(...)
   local args = { ... }
   if is_op(args[1]) then
      local opname = opsymb[args[1]]
      if #args == 3 then
         tremove(args, 1)
         return { tag = "Op", opname, unpack(args) }
      elseif #args == 2 then
         return {
            tag = "Paren",
            { tag = "Function", { Id "x" }, { { tag = "Return", { tag = "Op", opname, Id "x", args[2] } } } },
         }
      else
         return args[1]
      end
   end
   return rTails(args)
end

local function pTailop(...)
   local args = { ... }
   local symb = tremove(args, 1)
   args = pDollar(unpack(args))
   return function(x)
      return { tag = "Call", { tag = "Index", Id "op", Str(symb) }, x, args }
   end
end

local function pSubCycle(args, tag)
   args = map(pSeq(false), args)
   if tag == "Stack" then
      return Call("stack", Table(args))
   elseif tag == "Choose" then
      return Call("randcat", Table(args))
   elseif tag == "DotStack" then
      return Call("fastcat", Table(args))
   end
end

local function pPolymeterSteps(s)
   return (s ~= "") and s or -1
end

local function pPolymeter(args, _, steps)
   steps = (steps == -1) and Num(#args[1]) or steps
   args = map(pSeq(false), args)
   return Call("polymeter", steps, Table(args))
end

local function pSlowSeq(args, tag)
   if tag then
      args = map(pSeq(false), args)
      if tag == "DotStack" then
         return Call("slowcat", Table(args))
      elseif tag == "Choose" then
         return Call("randcat", Table(args))
      end
   end
   return pSeq(true)(rReps(args))
end

local function pRoot(...)
   local stats = { ... }
   for i, a in ipairs(stats) do
      stats[i] = a
   end
   ---@diagnostic disable-next-line: inject-field
   stats.tag = "Chunk"
   return stats
end

local function pRet(a)
   return { tag = "Return", a }
end

-- require "moon.all"
local function pSet(lhs, rhs)
   lhs.tag = "Id"
   return { tag = "Set", { lhs }, { rhs } }
end

local function pStat(...)
   if select("#", ...) == 1 then
      return pRet { ... }
   end
   if select(2, ...) == "=" then
      return pSet(select(1, ...), select(3, ...))
   end
   return pRet(rTails { ... })
end

local function pDot(...)
   return { ... }
end
local tab = V "tab"

local function pTab(...)
   return Table { ... }
end

local semi = P ";" ^ -1
local grammar = {
   [1] = "root",
   root = (ret * semi) ^ 1 / pRoot,
   ret = (list + mini + dollar) / pRet,
   list = ws * P "(" * ws * (expr + arith) * expr ^ 0 * ws * P ")" * ws / pList,
   tab = ws * P "'(" * ws * expr ^ 1 * ws * P ")" * ws / pTab,
   dollar = S "$>" * ws * step * ws * expr ^ 0 * ws / pDollar,
   expr = ws * (tab + mini + list + dollar + tailop) * ws,
   sequence = (mini ^ 1) / pDot,
   stack = sequence * (comma * sequence) ^ 0 / pStack,
   choose = sequence * (pipe * sequence) ^ 1 / pChoose,
   dotStack = sequence * (dot * sequence) ^ 1 / pDotStack,
   tailop = tidalop * ws * step * ws * mini * ws / pTailop,
   mini = (slice * op ^ 0) / pSlices,
   slice = step + number + sub_cycle + polymeter + slow_sequence + list,
   sub_cycle = P "[" * ws * (dotStack + choose + stack) * ws * P "]" / pSubCycle,
   slow_sequence = P "<" * ws * (dotStack + choose + sequence) * ws * P ">" / pSlowSeq,
   polymeter = P "{" * ws * stack * ws * P "}" * polymeter_steps * ws / pPolymeter,
   polymeter_steps = (P "%" * slice) ^ -1 / pPolymeterSteps,
   -- op = fast + slow + tail + range + replicate + degrade + weight + euclid + rand,
   op = fast + slow + tail + range + replicate + degrade + weight + euclid,
   fast = P "*" * slice / pFast,
   slow = P "/" * slice / pSlow,
   tail = P ":" * slice / pTail,
   range = P ".." * ws * slice / pRange,
   -- rand = P "#" * (number ^ -1) / pRand,
   degrade = P "?" * (number ^ -1) / pDegrade,
   replicate = ws * P "!" * (number ^ -1) / pReplicate,
   weight = ws * (P "@" + P "_") * (number ^ -1) / pWeight,
   euclid = P "(" * ws * mini * comma * mini * ws * comma ^ -1 * mini ^ -1 * ws * P ")" / pEuclid,
}

local function make_gen(top_level)
   if top_level then
      stat = expr * (P "=" / id) ^ -1 * expr ^ 0 * ws / pStat
      grammar.root = (stat * semi) ^ 1 / pRoot
   else
      grammar.root = (ret * semi) ^ 1 / pRoot
   end

   local rules = Ct(C(grammar))

   local function read(str)
      return rules:match(str)[2]
   end

   return function(env)
      local to_str = function(src)
         local ok, ast
         ok, ast = pcall(read, src)
         if not ok then
            return false
         end
         local lua_src = a2s.run(ast) -- TODO: imporve api
         return lua_src
      end

      local function to_f(src)
         if not top_level then
            src = "[" .. src .. "]"
         end
         local ok, fn
         local lua_src = to_str(src)
         -- print(lua_src)
         if not lua_src then
            return false
         end
         ok, fn = pcall(loadstring, lua_src)
         if not ok then
            return false
         end
         setfenv(fn and fn or function()
            print "not a valid maxi notation"
         end, env)
         return fn()
      end
      return memoize(to_f)
   end
end

notation = { maxi = make_gen(true), mini = make_gen(false) }

return notation
