local P, S, V, R, C, Ct, Cc
do
  local _obj_0 = require("lpeg")
  P, S, V, R, C, Ct, Cc = _obj_0.P, _obj_0.S, _obj_0.V, _obj_0.R, _obj_0.C, _obj_0.Ct, _obj_0.Cc
end
require("moon.all")
local tinsert, sequence, group, slice, sub_cycle, polymeter, slow_sequence, polymeter_steps, stack_tail, stack_or_choose, polymeter_stack, dot_tail, choose_tail, step, slice_with_ops, op, fast, slow, replicate, degrade, weight, euclid, tail, range, parseNumber, parseStep, AtomStub, PatternStub, ElementStub, seed, minus, plus, zero, digit, decimal_point, digit1_9, e, int, intneg, exp, frac, number, ws, comma, pipe, dot, quote, step_char, parseFast, parseSlow, parseTail, parseRange, parseDegrade, parseEuclid, parseWeight, parseReplicate, parseSlices, parsePolymeter, parseSlowSeq, parseDotTail, parseStackTail, parseChooseTail, parseStackOrChoose, parsePolymeterStack, parseSequence, grammar, parse
tinsert = table.insert
sequence = V("sequence")
group = V("group")
slice = V("slice")
sub_cycle = V("sub_cycle")
polymeter = V("polymeter")
slow_sequence = V("slow_sequence")
polymeter_steps = V("polymeter_steps")
stack_tail = V("stack_tail")
stack_or_choose = V("stack_or_choose")
polymeter_stack = V("polymeter_stack")
dot_tail = V("dot_tail")
choose_tail = V("dot_tail")
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
parseNumber = function(num)
  return tonumber(num)
end
parseStep = function(chars)
  if chars ~= "." and chars ~= "_" then
    return AtomStub(chars)
  end
end
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
  return setmetatable({
    type = "element",
    source = source,
    options = options
  }, {
    __tostring = function(self)
      return "ElementStub" .. tostring(self)
    end
  })
end
seed = -1
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
ws = S(" \n\r\t\u00A0") ^ 0
comma = ws * P(",") * ws
pipe = ws * P("|") * ws
dot = ws * P(".") * ws
quote = P("'") + P('"')
step_char = R("AZ", "az", "09") + P("-") + P("#") + P(".") + P("^") + P("_")
step = ws * (step_char ^ 1) / parseStep * ws
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
  s.arguments.stepsPerCycle = steps
  return s
end
parseSlowSeq = function(s, steps)
  s.arguments.alignment = "polymeter_slowcat"
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
parseStackTail = function(...)
  return {
    alignment = "stack",
    list = {
      ...
    }
  }
end
parseChooseTail = function(...)
  return {
    alignment = "rand",
    list = {
      ...
    },
    seed = seed + 1
  }
end
parseStackOrChoose = function(head, ...)
  tail = {
    ...
  }
  if tail and #tail > 0 then
    return PatternStub({
      head,
      unpack(tail)
    }, tail.alignment, tail.seed)
  else
    return head
  end
end
parsePolymeterStack = function(head, tail)
  return PatternStub(tail and {
    head,
    unpack(tail.list)
  } or {
    head
  }, "alignment")
end
parseSequence = function(...)
  return PatternStub({
    ...
  }, "fastcat")
end
grammar = {
  "stack_or_choose",
  stack_or_choose = (sequence * (stack_tail + choose_tail + dot_tail) ^ 0) / parseStackOrChoose,
  polymeter_stack = (sequence * stack_tail ^ -1) / parsePolymeterStack,
  sequence = (slice_with_ops ^ 1) / parseSequence,
  stack_tail = (comma * sequence) ^ 1 / parseStackTail,
  dot_tail = (dot * sequence) ^ 1,
  choose_tail = (pipe * sequence) ^ 1 / parseChooseTail,
  slice_with_ops = (slice * op ^ 0) / parseSlices,
  slice = step + sub_cycle + polymeter + slow_sequence,
  sub_cycle = P("[") * ws * stack_or_choose * ws * P("]"),
  polymeter = P("{") * ws * polymeter_stack * ws * P("}") * polymeter_steps ^ -1 * ws / parsePolymeter,
  slow_sequence = P("<") * ws * polymeter_stack * ws * P(">") * ws / parseSlowSeq,
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
