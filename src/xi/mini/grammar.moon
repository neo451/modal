--- defines the PEG grammar for parsing mini-notation
-- @module mini.grammar
import P, S, V, R, C, Ct, Cc from require("lpeg")

token = (id) -> Ct Cc(id) * C(V(id))
token_var = (id, patt) -> Ct Cc(id) * C(patt)

sequence = token "sequence"
group = token "group"
element = token "element"
m_element = token "m_element"
element_value = token "element_value"
polyrhythm_subseq = token "polyrhythm_subseq"
polymeter_subseq = token "polymeter_subseq"
polymeter1_subseq = token "polymeter1_subseq"
polymeter_steps = token "polymeter_steps"
subseq_body = token "subseq_body"
term = token "term"
word_with_index = token "word_with_index"
index = token "index"
euclid_modifier = token "euclid_modifier"
euclid_rotation_param = token "euclid_rotation_param"
modifiers = token "modifiers"
modifier = token "modifier"
fast = token "fast"
slow = token "slow"
repeat1 = token "repeat1"
repeatn = token "repeatn"
_repeat = token "_repeat"
degrade = token "degrade"
degrade1 = token "degrade1"
degrader = token "degrader"
degraden = token "degraden"
weight = token "weight"
word = token "word"
number = token "number"
real = token "real"
pos_real = token "pos_real"
integer = token "integer"
pos_integer = token "pos_integer"
rest = token "rest"
elongate = token "elongate"
other_groups = token_var "other", V("other_groups")
other_seqs = token_var "other", V("other_seqs")
other_subseqs = token_var "other", V("other_subseqs")
other_elements = token_var "other", V("other_elements")
minus = V "minus"
ws = V "ws"


--- table of PEG grammar rules
-- @table grammar
grammar = {
  "root", -- initial rule, root
  -- root
  root: token_var "root", ws ^ -1 * sequence * ws ^ -1,

    -- sequence
  sequence: group * other_groups * other_seqs,
  other_groups: (ws * -P("|") * P(".") * ws * group) ^ 0,
  other_seqs: (ws ^ -1 * P("|") * ws ^ -1 * sequence) ^ 0,
  group: element * other_elements,
  other_elements: (ws * -P(".") * element) ^ 0,

  -- element
  element: element_value * euclid_modifier * modifiers * elongate,
  -- tmp fix?
  m_element: element_value * euclid_modifier, --???euclidian neccssary?
  element_value: term + polyrhythm_subseq + polymeter_subseq + polymeter1_subseq,
  elongate: (ws ^ -1 * P("_")) ^ 0,

  -- subsequences
  polyrhythm_subseq: P("[") * ws ^ -1 * subseq_body * ws ^ -1 * P("]"),
  polymeter_subseq: P("{") * ws ^ -1 * subseq_body * ws ^ -1 * P("}") * polymeter_steps ^ -1,
  polymeter1_subseq: P("<") * ws ^ -1 * subseq_body * ws ^ -1 * P(">"),
  polymeter_steps: P("%") * number,
  subseq_body: sequence * other_subseqs,
  other_subseqs: (ws ^ -1 * P(",") * ws ^ -1 * sequence) ^ 0,

  -- terms
  term: number + word_with_index + rest,
  word_with_index: word * index ^ -1,
  index: P(":") * number,

  -- eculid modifier
  euclid_modifier: ( P("(") * ws ^ -1 * sequence * ws ^ -1 * P(",") * ws ^ -1 * sequence * euclid_rotation_param ^ -1 * ws ^ -1 * P(")")) ^ 0,
  euclid_rotation_param: ws ^ -1 * P(",") * ws ^ -1 * sequence,

  -- term modifiers
  modifiers: modifier ^ 0,
  modifier: fast + slow + _repeat + degrade + weight,
  fast: P("*") * m_element,
  slow: P("/") * m_element,
  _repeat: (repeat1 + repeatn) ^ 1,
  repeatn: P("!") * -P("!") * pos_integer,
  repeat1: P("!") * -pos_integer,
  degrade: degrade1 + degraden + degrader,
  degrader: P("?") * -P("?") * pos_real,
  degraden: P("?") * -pos_real * -P("?") * pos_integer,
  degrade1: P("?") * -pos_integer * -pos_real,
  weight: P("@") * number,

  -- primitives
  word: R("az", "AZ") ^ 1,
  number: real + integer,
  real: integer * P(".") * pos_integer ^ -1,
  pos_real: pos_integer * P(".") * pos_integer ^ -1,
  integer: minus ^ -1 * pos_integer,
  pos_integer: -minus * R("09") ^ 1,
  rest: P("~"),

  -- Misc
  minus: P("-"),
  ws: S(" \t") ^ -1,
}

grammar = Ct C grammar

--- Parse takes a string of mini code and returns an AST
-- @tparam string string of mini-notation
-- @treturn table table of AST nodes
parse = (string) -> grammar\match(string)[2]

return { parse: parse }
