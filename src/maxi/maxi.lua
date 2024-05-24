local P, S, V, R, C, Ct
do
   local _obj_0 = require("lpeg")
   P, S, V, R, C, Ct, Cc = _obj_0.P, _obj_0.S, _obj_0.V, _obj_0.R, _obj_0.C, _obj_0.Ct
end
-- local tinsert, sequence, group, slice, sub_cycle, polymeter, slow_sequence, polymeter_steps, stack, stack_or_choose, polymeter_stack, dotStack, choose, step, slice_with_ops, op, fast, slow, replicate, degrade, weight, euclid, tail, range, AtomStub, PatternStub, ElementStub, id, seed, ws, comma, pipe, dot, quote, parseNumber, parseStep, step_char, minus, plus, zero, digit, decimal_point, digit1_9, e, int, intneg, exp, frac, number, parseFast, parseSlow, parseTail, parseRange, parseDegrade, parseEuclid, parseWeight, parseReplicate, parseSlices, parsePolymeter, parseSlowSeq, parseDotTail, parseStack, parseDotStack, parseChoose, parseStackOrChoose, parseSubCycle, grammar

-- local pp = require("metalua.pprint").print
local moon = require("moon")
local tinsert = table.insert
local sequence = V("sequence")
local slice = V("slice")
local sub_cycle = V("sub_cycle")
local polymeter = V("polymeter")
local slow_sequence = V("slow_sequence")
local polymeter_steps = V("polymeter_steps")
local stack = V("stack")
local polymeter_stack = V("polymeter_stack")
local dotStack = V("dotStack")
-- local step = V("step")
local slice_with_ops = V("slice_with_ops")
local op = V("op")
local fast = V("fast")
local slow = V("slow")
local replicate = V("replicate")
local degrade = V("degrade")
local weight = V("weight")
local euclid = V("euclid")
local tail = V("tail")
local range = V("range")
local list = V("list")
local fn = V("fn")
local set = V("set")

Id = function(a)
   return { tag = "Id", a }
end

Table = function(a)
   return { tag = "Table", unpack(a) }
end

Str = function(a)
   return { tag = "String", a }
end

Num = function(a)
   return { tag = "Number", a }
end

local id = function(x)
   return x
end

local string2id = function(v)
   if v.tag == "String" then
      v.tag = "Id"
   end
   return v
end

local seed = -1
local ws = S(" \n\r\t") ^ 0
local comma = ws * P(",") * ws
-- pipe = ws * P("|") * ws
-- dot = ws * P(".") * ws

local parseNumber = function(num)
   return { tag = "Number", tonumber(num) }
end

local parseStep = function(chars)
   if tonumber(chars) then
      return { tag = "Number", tonumber(chars) }
   end
   if string.sub(chars, 0, 1) == "'" then
      return { tag = "Id", chars:sub(2, #chars) }
   end
   return { tag = "String", chars }
end

-- local step_char = R("09", "AZ", "az") + P("-") + P("#") + P(".") + P("^") + P("_") + P("~") / id
local step_char = R("09", "AZ", "az") + P("'") + P("-") + P("#") + P(".") + P("^") + P("_") + P("~") / id
local step = ws * (((step_char ^ 1) + P("+") + P("-") + P("*") + P("/") + P("%")) / parseStep) * ws - P(".")
local minus = P("-")
local plus = P("+")
local zero = P("0")
local digit = R("09")
local decimal_point = P(".")
local digit1_9 = R("19")
local e = S("eE")
local int = zero + (digit1_9 * digit ^ 0)
local exp = e * (minus + plus) ^ -1 * digit ^ 1
local frac = decimal_point * digit ^ 1
local number = (minus ^ -1 * int * frac ^ -1 * exp ^ -1) / parseNumber

local parseFast = function(a)
   return function(x)
      return { tag = "Call", Id("fast"), a, x }
   end
end

local parseSlow = function(a)
   return function(x)
      return { tag = "Call", Id("slow"), a, x }
   end
end

local parseDegrade = function(a)
   if a == "?" then
      a = Num(0.5)
   end
   return function(x)
      seed = seed + 1
      return { tag = "Call", Id("degradeBy"), a, x }
   end
end

local parseTail = function(s)
   return function(x)
      return tinsert(x.options.ops, {
         type = "tail",
         arguments = { element = s },
      })
   end
end

local parseRange = function(s)
   return function(x)
      return { tag = "Call", Id("iota"), x, s }
   end
end

local parseEuclid = function(p, s, r)
   r = r and r or Num(0)
   return function(x)
      return { tag = "Call", Id("euclid"), p, s, r, x }
   end
end

local parseWeight = function(a)
   return function(x)
      x.weight = (x.weight or 1) + (tonumber(a) or 2) - 1
      return x
   end
end

local parseReplicate = function(a)
   return function(x)
      x.reps = (x.reps or 1) + (tonumber(a) or 2) - 1
      return x
   end
end

local function resolvereps(ast)
   local res = {}
   for _, node in pairs(ast) do
      if node.reps then
         local reps = node.reps
         for _ = 1, reps do
            node.reps = nil
            res[#res + 1] = node
         end
      else
         res[#res + 1] = node
      end
   end
   return res
end

local parseSlices = function(sli, ...)
   local ops = { ... }
   sli.reps = 1
   sli.weight = 1

   for i = 1, #ops do
      sli = ops[i](sli)
   end
   return sli
end

local pseq = function(...)
   local args = { ... }
   args = resolvereps(args)
   return args, false
end

local pstack = function(...)
   local args = { ... }
   args = resolvereps(args)
   return args, true
end

local reduce = require("modal.utils").reduce

local use_timecat = function(args)
   local addWeight = function(a, b)
      b = b.weight and b.weight or 1
      return a + b
   end
   local weightSum = reduce(addWeight, 0, args)
   if weightSum > #args then
      return true
   end
end

local resolveweight = function(args)
   local addWeight = function(a, b)
      return a + (b.weight and b.weight or 1)
   end
   local weightSum = reduce(addWeight, 0, args)
   local acc = {}
   for i, v in pairs(args) do
      acc[i] = Table({ Num(v.weight) or Num(1), args[i] })
   end
   return { tag = "Call", Id("timecat"), Table(acc) }, weightSum
end

local parseSubCycle = function(args, isStack)
   if isStack then
      return { tag = "Call", Id("stack"), unpack(args) }
   else
      if use_timecat(args) then
         -- pp(args)
         local res = resolveweight(args)
         return res
      else
         return { tag = "Call", Id("fastcat"), unpack(args) }
      end
   end
end

local parsePolymeter = function(s, steps)
   steps = steps and steps or Num(#s)
   return { tag = "Call", Id("polymeter"), steps, unpack(s) }
end

local parseSlowSeq = function(args, _)
   if use_timecat(args) then
      local tab, weightSum = resolveweight(args)
      return { tag = "Call", Id("slow"), Num(weightSum), tab }
   else
      return { tag = "Call", Id("slowcat"), unpack(args) }
   end
end

local pset = function(lhs, rhs)
   return { tag = "Set", { string2id(lhs) }, { rhs } }
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

local function plist(...)
   local args = { ... }
   if #args == 3 then
      if is_op(args[2][1]) then
         local opname = opsymb[args[2][1]]
         table.remove(args, 2)
         return { tag = "Op", opname, unpack(args) }
      elseif is_op(args[1][1]) then
         local opname = opsymb[args[1][1]]
         table.remove(args, 1)
         return { tag = "Op", opname, unpack(args) }
      end
   end
   local fname = args[1]
   fname.tag = "Id"
   table.remove(args, 1)
   return { tag = "Call", fname, unpack(args) }
end

-- TODO: weight in polymeter
-- TODO: quotes?
-- TODO: to fraction ?
-- TODO:  code blocks

local grammar =
   {
      "root",
      -- root = fn + set + list + slice_with_ops,
      root = set + list + slice_with_ops,
      -- fn = P"fn" * ws * step * param * body,
      list = P("(") * ws * (step + list + slice_with_ops) ^ 0 * ws * P(")") / plist,
      -- + dotStack + choose + stack + sequence,
      set = step * ws * P("=") * ws * (step + list + slice_with_ops) / pset,
      sequence = (slice_with_ops ^ 1) / pseq,
      stack = slice_with_ops * (comma * slice_with_ops) ^ 1 / pstack,
      -- choose = sequence * (pipe * sequence) ^ 1 / parseChoose,
      -- dotStack = sequence * (dot * sequence) ^ 1 / parseDotStack,
      slice_with_ops = (slice * op ^ 0) / parseSlices,
      slice = step + sub_cycle + polymeter + slow_sequence,
      sub_cycle = P("[") * ws * (stack + sequence) * ws * P("]") / parseSubCycle,
      -- sub_cycle = P("[") * ws * sequence * ws * P("]") / parseSubCycle,
      slow_sequence = P("<") * ws * sequence * ws * P(">") / parseSlowSeq,
      polymeter = P("{") * ws * sequence * ws * P("}") * polymeter_steps ^ -1 * ws / parsePolymeter,
      polymeter_steps = P("%") * slice,
      op = fast + slow + tail + range + replicate + degrade + weight + euclid,
      fast = P("*") * slice / parseFast,
      slow = P("/") * slice / parseSlow,
      tail = P(":") * slice / parseTail,
      range = P("..") * ws * slice / parseRange,
      degrade = P("?") * (number ^ -1) / parseDegrade,
      replicate = ws * P("!") * (number ^ -1) / parseReplicate,
      weight = ws * (P("@") + P("_")) * (number ^ -1) / parseWeight,
      euclid = P("(") * ws * slice_with_ops * comma * slice_with_ops * ws * comma ^ -1 * slice_with_ops ^ -1 * ws * P(
         ")"
      ) / parseEuclid,
   }

grammar = Ct(C(grammar))

local read = function(str)
   return grammar:match(str)[2]
end

local mlc = require("metalua.compiler").new()

local mpp = require("metalua.pprint").print

local function eval(src, env)
   env = env and env or _G
   local ast = read(src)
   -- mpp(ast)
   ast = { tag = "Return", ast }

   local lua_src = mlc:ast_to_src(ast)
   local f = loadstring(lua_src)
   f = setfenv(f, _G)
   return f()
end

-- setfenv

if _VERSION == "Lua 5.2" then
   function setfenv(f, env)
      return load(string.dump(f), nil, nil, env)
   end
end

local function to_lua(src)
   local ast = read(src)
   local lua_src = mlc:ast_to_src(ast)
   return lua_src
end

-- print(eval("[ 37 'a ]"))

return { eval = eval, read = read, to_lua = to_lua }
