local P, S, V, R, C, Ct, Cc
do
   local _obj_0 = require("lpeg")
   P, S, V, R, C, Ct, Cc = _obj_0.P, _obj_0.S, _obj_0.V, _obj_0.R, _obj_0.C, _obj_0.Ct, _obj_0.Cc
end
local tinsert, sequence, group, slice, sub_cycle, polymeter, slow_sequence, polymeter_steps, stack, stack_or_choose, polymeter_stack, dotStack, choose, step, slice_with_ops, op, fast, slow, replicate, degrade, weight, euclid, tail, range, AtomStub, PatternStub, ElementStub, id, seed, ws, comma, pipe, dot, quote, parseNumber, parseStep, step_char, minus, plus, zero, digit, decimal_point, digit1_9, e, int, intneg, exp, frac, number, parseFast, parseSlow, parseTail, parseRange, parseDegrade, parseEuclid, parseWeight, parseReplicate, parseSlices, parsePolymeter, parseSlowSeq, parseDotTail, parseStack, parseDotStack, parseChoose, parseStackOrChoose, parseSequence, parseSubCycle, grammar, parse
require("moon.all")
tinsert = table.insert
sequence = V("sequence")
group = V("group")
slice = V("slice")
sub_cycle = V("sub_cycle")
polymeter = V("polymeter")
slow_sequence = V("slow_sequence")
polymeter_steps = V("polymeter_steps")
stack = V("stack")
stack_or_choose = V("stack_or_choose")
polymeter_stack = V("polymeter_stack")
dotStack = V("dotStack")
choose = V("choose")
step = V("step")
slice_with_ops = V("slice_with_ops")
op = V("op")
fast = V("fast")
slow = V("slow")
replicate = V("replicate")
degrade = V("degrade")
weight = V("weight")
euclid = V("euclid")
tail = V("tail")
range = V("range")

local map = require("modal.utils").map

AtomStub = function(source)
   return { type = "atom", source = source }
end

PatternStub = function(source, alignment, seed)
   return {
      type = "pattern",
      arguments = { alignment = alignment, seed = seed },
      source = source,
   }
end
ElementStub = function(source, options)
   return { type = "element", source = source, options = options }
end

id = function(x)
   return x
end
seed = -1
ws = S(" \n\r\t") ^ 0
comma = ws * P(",") * ws
pipe = ws * P("|") * ws
dot = ws * P(".") * ws
quote = P("'") + P('"')
parseNumber = function(num)
   return tonumber(num)
end
parseStep = function(chars)
   if chars ~= "." and chars ~= "_" then
      return AtomStub(chars)
   end
end
step_char = R("09", "AZ", "az") + P("-") + P("#") + P(".") + P("^") + P("_") + P("~") / id
step = ws * (step_char ^ 1 / parseStep) * ws - P(".")
minus = P("-")
plus = P("+")
zero = P("0")
digit = R("09")
decimal_point = P(".")
digit1_9 = R("19")
e = S("eE")
int = zero + (digit1_9 * digit ^ 0)
intneg = minus ^ -1 * int
exp = e * (minus + plus) ^ -1 * digit ^ 1
frac = decimal_point * digit ^ 1
number = (minus ^ -1 * int * frac ^ -1 * exp ^ -1) / parseNumber
parseFast = function(a)
   return function(x)
      return tinsert(x.options.ops, {
         type = "stretch",
         arguments = { amount = a, type = "fast" },
      })
   end
end
parseSlow = function(a)
   return function(x)
      return tinsert(x.options.ops, {
         type = "stretch",
         arguments = { amount = a, type = "slow" },
      })
   end
end
parseTail = function(s)
   return function(x)
      return tinsert(x.options.ops, {
         type = "tail",
         arguments = { element = s },
      })
   end
end
parseRange = function(s)
   return function(x)
      return tinsert(x.options.ops, {
         type = "range",
         arguments = { element = s },
      })
   end
end
parseDegrade = function(a)
   if type(a) == "number" then
      a = tonumber(a)
   else
      a = nil
   end
   return function(x)
      seed = seed + 1
      return tinsert(x.options.ops, {
         type = "degradeBy",
         arguments = { amount = a, seed = seed },
      })
   end
end
parseEuclid = function(p, s, r)
   if r == nil then
      r = 0
   end
   return function(x)
      return tinsert(x.options.ops, {
         type = "euclid",
         arguments = { pulse = p, steps = s, rotation = r },
      })
   end
end
parseWeight = function(a)
   return function(x)
      x.options.weight = (x.options.weight or 1) + (tonumber(a) or 2) - 1
   end
end

parseReplicate = function(a)
   return function(x)
      x.options.reps = (x.options.reps or 1) + (tonumber(a) or 2) - 1
   end
end
parseSlices = function(slice, ...)
   local ops = { ... }
   local result = ElementStub(slice, {
      ops = {},
      weight = 1,
      reps = 1,
   })
   for _index_0 = 1, #ops do
      local op = ops[_index_0]
      op(result)
   end
   return result
end
parsePolymeter = function(s, steps)
   s = PatternStub({
      s,
   }, "polymeter")
   s.arguments.stepsPerCycle = steps
   return s
end
parseSlowSeq = function(s, steps)
   s = PatternStub({ s }, "polymeter_slowcat")
   return s
end

parseStack = function(...)
   return PatternStub({ ... }, "stack")
end
parseDotStack = function(...)
   seed = seed + 1
   return PatternStub({ ... }, "feet", seed)
end
parseChoose = function(...)
   seed = seed + 1
   return PatternStub({ ... }, "rand", seed)
end

parseSequence = function(...)
   return PatternStub({ ... }, "fastcat")
end

parseSubCycle = function(s)
   return s
end

grammar =
   {
      "root",
      root = dotStack + choose + stack + sequence,
      sequence = (slice_with_ops ^ 1) / parseSequence,
      stack = sequence * (comma * sequence) ^ 1 / parseStack,
      choose = sequence * (pipe * sequence) ^ 1 / parseChoose,
      dotStack = sequence * (dot * sequence) ^ 1 / parseDotStack,
      slice_with_ops = (slice * op ^ 0) / parseSlices,
      slice = step + sub_cycle + polymeter + slow_sequence,
      sub_cycle = P("[") * ws * (stack + sequence) * ws * P("]") / parseSubCycle,
      polymeter = P("{") * ws * sequence * ws * P("}") * polymeter_steps ^ -1 * ws / parsePolymeter,
      slow_sequence = P("<") * ws * sequence * ws * P(">") * ws / parseSlowSeq,
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

parse = function(str)
   return grammar:match(str)[2]
end

local function applyOptions(parent, enter)
   return function(pat, i)
      local ast = parent.source[i]
      local ops = nil
      if ast.options then
         ops = ast.options.ops
      end
      if ops then
         for _index_0 = 1, #ops do
            local op = ops[_index_0]
            local _exp_0 = op.type
            if "stretch" == _exp_0 then
               local type_, amount = op.arguments.type, op.arguments.amount
               local _exp_1 = type_
               if "fast" == _exp_1 then
                  pat = fast(enter(amount), pat)
               elseif "slow" == _exp_1 then
                  pat = slow(enter(amount), pat)
               else
                  print("mini: stretch: type must be one of fast of slow")
               end
            elseif "degradeBy" == _exp_0 then
               local amount = op.arguments.amount or 0.5
               pat = degradeBy(amount, pat)
            elseif "euclid" == _exp_0 then
               local steps, pulse, rotation = op.arguments.steps, op.arguments.pulse, op.arguments.rotation
               if rotation == 0 then
                  pat = euclid(enter(pulse), enter(steps), 0, pat)
               else
                  pat = euclid(enter(pulse), enter(steps), enter(rotation), pat)
               end
            elseif "tail" == _exp_0 then
               local friend = enter(op.arguments.element)
               pat = appLeft(
                  fmap(pat, function(a)
                     return function(b)
                        if T(a) == "table" then
                           tinsert(a, b)
                           return a
                        else
                           return {
                              a,
                              b,
                           }
                        end
                     end
                  end),
                  friend
               )
               --- TODO: broken!
            elseif "range" == _exp_0 then
               local friend = enter(op.arguments.element)
               local makeRange
               makeRange = function(start, stop)
                  local _accum_0 = {}
                  local _len_0 = 1
                  for _ = start, stop do
                     _accum_0[_len_0] = i
                     _len_0 = _len_0 + 1
                  end
                  return _accum_0
               end
               local f
               f = function(apat, bpat)
                  return squeezeBind(apat, function(a)
                     return bind(bpat, function(b)
                        return fastcat(makeRange(a, b), friend)
                     end)
                  end)
               end
               pat = f(pat, friend)
            end
         end
      end
      return pat
   end
end

local function resolveReplications(ast)
   local repChild = function(child)
      if child.options == nil then
         return { child }
      end
      local reps = child.options.reps
      child.options.reps = nil
      local _accum_0 = {}
      local _len_0 = 1
      for i = 1, reps do
         _accum_0[_len_0] = child
         _len_0 = _len_0 + 1
      end
      return _accum_0
   end
   local unflat
   do
      local _accum_0 = {}
      local _len_0 = 1
      local _list_0 = ast.source
      for _index_0 = 1, #_list_0 do
         local child = _list_0[_index_0]
         _accum_0[_len_0] = repChild(child)
         _len_0 = _len_0 + 1
      end
      unflat = _accum_0
   end
   local res = {}
   for _index_0 = 1, #unflat do
      local element = unflat[_index_0]
      for _index_1 = 1, #element do
         local elem = element[_index_1]
         tinsert(res, elem)
      end
   end
   ast.source = res
   return ast
end

local function patternifyAST(ast, M)
   local enter = function(node)
      return patternifyAST(node)
   end
   local _exp_0 = ast.type
   if "element" == _exp_0 then
      return enter(ast.source)
   elseif "atom" == _exp_0 then
      if ast.source == "~" then
         return silence()
      end
      local value = ast.source
      if tonumber(value) then
         value = tonumber(value)
      end
      return pure(value)
   elseif "pattern" == _exp_0 then
      ast = resolveReplications(ast)
      local children = ast.source
      children = map(enter, children)
      do
         local _accum_0 = {}
         local _len_0 = 1
         for index, child in pairs(children) do
            _accum_0[_len_0] = applyOptions(ast, enter)(child, index)
            _len_0 = _len_0 + 1
         end
         children = _accum_0
      end
      local alignment = ast.arguments.alignment
      local _exp_1 = alignment
      if "stack" == _exp_1 then
         return stack(children)
      elseif "polymeter_slowcat" == _exp_1 then
         local aligned = map(function(child)
            return slow(#(firstCycle(child)), child)
         end, children)
         return stack(aligned)
      elseif "polymeter" == _exp_1 then
         local stepsPerCycle = ast.arguments.stepsPerCycle and enter(ast.arguments.stepsPerCycle) or 1
         print(stepsPerCycle)
         local aligned = map(function(child)
            return fast(
               fmap(reify(stepsPerCycle), function(x)
                  if x == 1 then
                     return 1
                  end -- HACK:
                  return x / #(firstCycle(child))
               end),
               child
            )
         end, children)
         return stack(aligned)
      elseif "rand" == _exp_1 then
         return randcat(children)
      end
      local addWeight
      addWeight = function(a, b)
         b = b.options and b.options.weight or 1
         return a + b
      end
      local weightSum = reduce(addWeight, 0, ast.source)
      if weightSum > #children then
         local atoms = ast.source
         local pat = timecat((function()
            local _accum_0 = {}
            local _len_0 = 1
            for i, v in pairs(atoms) do
               _accum_0[_len_0] = {
                  v.options.weight or 1,
                  children[i],
               }
               _len_0 = _len_0 + 1
            end
            return _accum_0
         end)())
         return pat
      end
      return fastcat(children)
   end
end

return function(M)
   return function(code)
      local ast = parse(code)
      setfenv(patternifyAST, M)
      return patternifyAST(ast)
   end
end
