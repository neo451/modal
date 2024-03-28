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
AtomStub = function(source)
  return {
    type = "atom",
    source = source
  }
end
PatternStub = function(source, alignment, seed)
  return {
    type = "pattern",
    arguments = {
      alignment = alignment,
      seed = seed
    },
    source = source
  }
end
ElementStub = function(source, options)
  return {
    type = "element",
    source = source,
    options = options
  }
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
step_char = R("09", "AZ", "az") + P("-") + P("#") + P(".") + P("^") + P("_") / id
step = ws * (step_char ^ 1 / parseStep) * ws
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
      arguments = {
        amount = a,
        type = "fast"
      }
    })
  end
end
parseSlow = function(a)
  return function(x)
    return tinsert(x.options.ops, {
      type = "stretch",
      arguments = {
        amount = a,
        type = "slow"
      }
    })
  end
end
parseTail = function(s)
  return function(x)
    return tinsert(x.options.ops, {
      type = "tail",
      arguments = {
        element = s
      }
    })
  end
end
parseRange = function(s)
  return function(x)
    return tinsert(x.options.ops, {
      type = "range",
      arguments = {
        element = s
      }
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
      arguments = {
        amount = a,
        seed = seed
      }
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
      arguments = {
        pulse = p,
        steps = s,
        rotation = r
      }
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
  local ops = {
    ...
  }
  local result = ElementStub(slice, {
    ops = { },
    weight = 1,
    reps = 1
  })
  for _index_0 = 1, #ops do
    local op = ops[_index_0]
    op(result)
  end
  return result
end
parsePolymeter = function(s, steps)
  s = PatternStub({
    s
  }, "polymeter")
  s.arguments.stepsPerCycle = steps
  return s
end
parseSlowSeq = function(s, steps)
  s = PatternStub({
    s
  }, "polymeter_slowcat")
  return s
end
parseDotTail = function(...)
  return {
    alignment = "feet",
    list = {
      ...
    },
    seed = seed + 1
  }
end
parseStack = function(...)
  return PatternStub({
    ...
  }, "stack")
end
parseDotStack = function(...)
  p(...)
  seed = seed + 1
  return PatternStub({
    ...
  }, "feet", seed)
end
parseChoose = function(...)
  seed = seed + 1
  return PatternStub({
    ...
  }, "rand", seed)
end
parseStackOrChoose = function(head, tail)
  if tail and #tail.list > 0 then
    return PatternStub({
      head,
      unpack(tail.list)
    }, tail.alignment, tail.seed)
  else
    return head
  end
end
parseSequence = function(...)
  return PatternStub({
    ...
  }, "fastcat")
end
parseSubCycle = function(s)
  return s
end
grammar = {
  "root",
  root = choose + dotStack + sequence + stack,
  sequence = (slice_with_ops ^ 1) / parseSequence,
  stack = sequence * (comma * sequence) ^ 0 / parseStack,
  choose = sequence * (pipe * sequence) ^ 1 / parseChoose,
  dotStack = sequence * (dot * sequence) ^ 1 / parseDotStack,
  slice_with_ops = (slice * op ^ 0) / parseSlices,
  slice = step + sub_cycle + polymeter + slow_sequence,
  sub_cycle = P("[") * ws * stack * ws * P("]") / parseSubCycle,
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
  euclid = P("(") * ws * slice_with_ops * comma * slice_with_ops * ws * comma ^ -1 * slice_with_ops ^ -1 * ws * P(")") / parseEuclid
}
grammar = Ct(C(grammar))
parse = function(string)
  return grammar:match(string)[2]
end
return {
  parse = parse
}
