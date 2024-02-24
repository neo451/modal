--- defines the PEG grammar for parsing mini-notation
-- @module mini.grammar
import P, S, V, R, C, Ct, Cc from require("lpeg")
require "moon.all"
-- TODO: support musical notation like e3, cmaj3
token = (id) -> Ct Cc(id) * C(V(id))
token_var = (id, patt) -> Ct Cc(id) * C(patt)

sequence = token "sequence"
group = token "group"
-- element = token "element"
-- m_element = token "m_element"
slice = token "slice"
sub_cycle = token "sub_cycle"
polymeter = token "polymeter"
slow_sequence = token "slow_sequence"
polymeter_steps = token "polymeter_steps"
subseq_body = token "subseq_body"
step = token "step"
-- word_with_index = token "word_with_index"
-- index = token "index"
slice_with_ops = token "slice_with_ops"
-- ops = token "ops"
op = token "op"
fast = token "fast"
slow = token "slow"
r1 = token "r1"
rn = token "rn"
replicate = token "replicate"
degrade = token "degrade"
degrade1 = token "degrade1"
degrader = token "degrader"
degraden = token "degraden"
weight = token "weight"
euclid = token "euclid"
euclid_rotation = token "euclid_rotation"
tail = token "tail"
range = token "range"
word = token "word"
number = token "number"
real = token "real"
pos_real = token "pos_real"
integer = token "integer"
pos_integer = token "pos_integer"
rest = token "rest"
elongate = token "elongate"
-- other_groups = token_var "other", V("other_groups")
-- other_seqs = token_var "other", V("other_seqs")
-- other_subseqs = token_var "other", V("other_subseqs")
-- other_elements = token_var "other", V("other_elements")
-- chordmod = V "chordmod"
-- chordname = V "chordname"
minus = V "minus"
ws = V "ws"


--- table of PEG grammar rules
-- @table grammar
grammar = {
  "root", -- initial rule, root
  -- root
  root: token_var "root", ws ^ -1 * sequence * ws ^ -1,

  stack_or_choose: sequence * (stack_tail + choose_tail + dot_tail) ^ -1
  polymeter_stack: sequence * stack_tail ^ -1

  -- sequence
  -- sequence: group * other_groups * other_seqs,
  sequence: slice_with_ops ^ 1
  stack_tail: (comma * sequence) ^ 1 -- ??
  dot_tail: (dot * sequence) ^ 1 -- ??
  choose_tail: (pipe * sequence) ^ 1 -- ??
  -- other_groups: (ws * -P("|") * P(".") * ws * group) ^ 0,
  -- other_seqs: (ws ^ -1 * P("|") * ws ^ -1 * sequence) ^ 0,
  -- group: slice_with_ops * other_elements,
  -- other_elements: (ws * -P(".") * slice_with_ops) ^ 0,

  -- element
  slice_with_ops: slice * op ^ 0
  -- tmp fix?
  -- m_element: element_value * euclid, --???euclidian neccssary?
  slice: step + sub_cycle + polymeter + slow_sequence

  -- subsequences
  sub_cycle: P("[") * ws ^ -1 * subseq_body * ws ^ -1 * P("]"),
  polymeter: P("{") * ws ^ -1 * subseq_body * ws ^ -1 * P("}") * polymeter_steps ^ -1,
  slow_sequence: P("<") * ws ^ -1 * subseq_body * ws ^ -1 * P(">"),
  polymeter_steps: P("%") * slice,
  subseq_body: sequence * other_subseqs,
  -- other_subseqs: (ws ^ -1 * P(",") * ws ^ -1 * sequence) ^ 0,

  -- step
  step: word + rest + number
  -- index: P(":") * number
  -- word_with_index: word * index ^ -1

  -- eculid modifier

  -- term modifiers
  op: fast + slow + replicate + degrade + weight + euclid + tail + range
  fast: P("*") * slice
  slow: P("/") * slice
  replicate: (r1 + rn) ^ 1
  rn: P("!") * -P("!") * pos_integer
  r1: P("!") * -pos_integer
  degrade: degrade1 + degraden + degrader
  degrader: P("?") * -P("?") * pos_real
  degraden: P("?") * -pos_real * -P("?") * pos_integer
  degrade1: P("?") * -pos_integer * -pos_real
  -- elongate: (ws ^ -1 * P("_")) ^ 0,
  -- weight: P("@") * number
  weight: ws * (P("@") + P("_")) * number
  euclid: P("(") * ws * slice_with_ops * ws * comma * slice_with_ops * ws * comma ^ -1 * slice_with_ops ^ -1 * ws * P(")")
  tail: P(":") * slice
  range: P("..") * slice

  -- primitives
  word: R("az", "AZ") ^ 1 * R("09") ^ -1 * P("'") ^ -1 * chordname ^ -1 * chordmod ^ 0
  chordname: R("az","09") ^ 1
  chordmod: P("'") * ((S"id" + R"09") + P"o" + R"09")
  number: real + integer
  real: integer * P(".") * pos_integer ^ -1
  pos_real: pos_integer * P(".") * pos_integer ^ -1
  integer: minus ^ -1 * pos_integer
  pos_integer: -minus * R("09") ^ 1
  rest: P("~")

  -- Misc
  minus: P("-")

  -- delimiters
  ws: S(" \n\r\t\u00A0") ^ 0
  comma: ws * P(",") * ws
  pipe: ws * P("|") * ws
  dot: ws * P(".") * ws
  quote: P("'") + P('"')
}

grammar = Ct C grammar

--- Parse takes a string of mini code and returns an AST
-- @tparam string string of mini-notation
-- @treturn table table of AST nodes
parse = (string) -> grammar\match(string)[2]

-- p parse "45"

return { :parse }
