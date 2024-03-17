--- defines the PEG grammar for parsing mini-notation
-- @module mini.grammar
import P, S, V, R, C, Ct, Cc from require("lpeg")
require "moon.all"

local *

-- unpack = unpack or table.unpack
tinsert = table.insert

sequence = V "sequence"
group = V "group"
slice = V "slice"
sub_cycle = V "sub_cycle"
polymeter = V "polymeter"
slow_sequence = V "slow_sequence"
polymeter_steps = V "polymeter_steps"
stack_tail = V "stack_tail"
stack_or_choose = V "stack_or_choose"
polymeter_stack = V "polymeter_stack"
dot_tail = V "dot_tail"
choose_tail = V "dot_tail"
step = V "step"
slice_with_ops = V "slice_with_ops"
op = V "op"
fast = V "fast"
slow = V "slow"
replicate = V "replicate"
degrade = V "degrade"
weight = V "weight"
euclid = V "euclid"
tail = V "tail"
range = V "range"

parseNumber = (num) -> tonumber num
parseStep = (chars) -> if chars != "." and chars != "_" then return AtomStub chars

AtomStub = (source) ->
  {
    type: "atom"
    source: source
    -- location: location() --?
  }

PatternStub = (source, alignment, seed) ->
  {
    type: "pattern"
    arguments: { alignment: alignment, seed: seed } -- ? what condition?
    source: source
  }

ElementStub = (source, options) ->
  return setmetatable({
    type: "element"
    source: source
    options: options
    -- location: location! -- ?
  },
  {
    __tostring: () => "ElementStub" .. tostring(@)
  }
  )


seed = 0 -- neccesary???

--- non-recersive rules
-- numbers
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

-- delimiters
ws = S" \n\r\t\u00A0" ^ 0
comma = ws * P"," * ws
pipe = ws * P"|" * ws
dot = ws * P"." * ws
quote = P"'" + P'"'

-- chars
step_char = R("AZ", "az", "09") + P"-" + P"#" + P"." + P"^" + P"_" -- upgrade to unicode
step = ws * (step_char ^ 1) / parseStep * ws
rest = P"~"

parseFast = (a) ->
  (x) -> tinsert x.options.ops, { type: "stretch", arguments: { amount: a, type: "fast" } }

parseSlow = (a) ->
  (x) -> tinsert x.options.ops, { type: "stretch", arguments: { amount: a, type: "slow" } }

parseTail = (s) ->
  (x) -> tinsert x.options.ops, { type: "tail", arguments: { element: s } }

parseRange = (s) ->
  (x) -> tinsert x.options.ops, { type: "range", arguments: { element: s } }

parseDegrade = (a) ->
  (x) -> tinsert x.options.ops, { type: "degradeBy", arguments: { amount: a, seed: seed + 1 } }

parseEuclid = (p, s, r = 0) ->
  (x) -> tinsert x.options.ops, { type: "euclid", arguments: { pulse: p, steps: s, rotation: r } }

parseWeight = (a) ->
  (x) -> x.options.weight = ( x.options.weight or 1 ) + ( tonumber(a) or 2 ) - 1

parseReplicate = (a) ->
  (x) -> x.options.reps = ( x.options.reps or 1 ) + ( tonumber(a) or 2 ) - 1

parseSlices = (slice, ...) ->
  ops = { ... }
  result = ElementStub(slice, { ops: {}, weight: 1, reps: 1})
  for op in *ops
    op(result)
  return result

parsePolymeter = (s, steps) ->
  s.arguments.stepsPerCycle = steps
  return s

parseSlowSeq = (s, steps) ->
  s.arguments.alignment = "polymeter_slowcat"
  return s

parseDotTail = (...) -> { alignment: "feet", list: { ... }, seed: seed + 1 }

parseStackTail = (...) -> { alignment: "stack", list: { ... } }

parseChooseTail = (...) -> { alignment: "rand", list: { ... }, seed: seed + 1 }

parseStackOrChoose = (head, tail) ->
  -- p tail
  if tail and #tail.list > 0
    return PatternStub({ head, unpack(tail.list) }, tail.alignment, tail.seed)
  else
    return head

parsePolymeterStack = (head, tail) ->
    PatternStub(tail and { head, unpack(tail.list) } or { head }, "alignment")

parseSequence = (...) -> 
  PatternStub({ ... }, "fastcat")

--- table of PEG grammar rules
-- @table grammar
grammar = {
  "stack_or_choose", -- initial rule

  stack_or_choose: (sequence * (stack_tail + choose_tail + dot_tail) ^ -1) / parseStackOrChoose
  polymeter_stack: (sequence * stack_tail ^ -1) / parsePolymeterStack

  -- sequence and tail
  sequence: (slice_with_ops ^ 1) / parseSequence
  stack_tail: (comma * sequence) ^ 1 / parseStackTail
  dot_tail: (dot * sequence) ^ 1 
  -- / parseDotTail
  choose_tail: (pipe * sequence) ^ 1 / parseChooseTail

  -- slices
  slice_with_ops: (slice * op ^ 0) / parseSlices
  slice: step + sub_cycle + polymeter + slow_sequence
  sub_cycle: P"[" * ws * stack_or_choose * ws * P"]" -- TODO:
  polymeter: P"{" * ws * polymeter_stack * ws * P"}" * polymeter_steps ^ -1 * ws / parsePolymeter
  slow_sequence: P"<" * ws * polymeter_stack * ws * P">" * ws / parseSlowSeq
  polymeter_steps: P"%" * slice

  -- ops
  op: fast + slow + tail + range + replicate + degrade + weight + euclid
  fast: P"*" * slice / parseFast
  slow: P"/" * slice / parseSlow
  tail: P":" * slice / parseTail
  range: P".." * ws * slice / parseRange
  degrade: P"?" * (number ^ -1) / parseDegrade
  replicate: ws * P"!" * (number ^ -1) / parseReplicate
  weight: ws * (P"@" + P"_") * (number ^ -1) / parseWeight
  euclid: P"(" * ws * slice_with_ops * comma * slice_with_ops * ws * comma ^ -1 * slice_with_ops ^ -1 * ws * P")" / parseEuclid
}

grammar = Ct C grammar

--- Parse takes a string of mini code and returns an AST
-- @tparam string string of mini-notation
-- @treturn table table of AST nodes
parse = (string) -> grammar\match(string)[2]

p parse("bd _ _ sd")

return { :parse }
