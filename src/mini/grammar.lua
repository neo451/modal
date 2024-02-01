local P, S, V, R, C, Ct, Cc
do
  local _obj_0 = require("lpeg")
  P, S, V, R, C, Ct, Cc = _obj_0.P, _obj_0.S, _obj_0.V, _obj_0.R, _obj_0.C, _obj_0.Ct, _obj_0.Cc
end
require("moon.all")
local token
token = function(id)
  return Ct(Cc(id) * C(V(id)))
end
local token_var
token_var = function(id, patt)
  return Ct(Cc(id) * C(patt))
end
local sequence = token("sequence")
local group = token("group")
local element = token("element")
local m_element = token("m_element")
local element_value = token("element_value")
local polyrhythm_subseq = token("polyrhythm_subseq")
local polymeter_subseq = token("polymeter_subseq")
local polymeter1_subseq = token("polymeter1_subseq")
local polymeter_steps = token("polymeter_steps")
local subseq_body = token("subseq_body")
local term = token("term")
local word_with_index = token("word_with_index")
local index = token("index")
local euclid_modifier = token("euclid_modifier")
local euclid_rotation_param = token("euclid_rotation_param")
local modifiers = token("modifiers")
local modifier = token("modifier")
local fast = token("fast")
local slow = token("slow")
local repeat1 = token("repeat1")
local repeatn = token("repeatn")
local _repeat = token("_repeat")
local degrade = token("degrade")
local degrade1 = token("degrade1")
local degrader = token("degrader")
local degraden = token("degraden")
local weight = token("weight")
local word = token("word")
local number = token("number")
local real = token("real")
local pos_real = token("pos_real")
local integer = token("integer")
local pos_integer = token("pos_integer")
local rest = token("rest")
local elongate = token("elongate")
local other_groups = token_var("other", V("other_groups"))
local other_seqs = token_var("other", V("other_seqs"))
local other_subseqs = token_var("other", V("other_subseqs"))
local other_elements = token_var("other", V("other_elements"))
local chordmod = V("chordmod")
local chordname = V("chordname")
local minus = V("minus")
local ws = V("ws")
local grammar = {
  "root",
  root = token_var("root", ws ^ -1 * sequence * ws ^ -1),
  sequence = group * other_groups * other_seqs,
  other_groups = (ws * -P("|") * P(".") * ws * group) ^ 0,
  other_seqs = (ws ^ -1 * P("|") * ws ^ -1 * sequence) ^ 0,
  group = element * other_elements,
  other_elements = (ws * -P(".") * element) ^ 0,
  element = element_value * euclid_modifier * modifiers * elongate,
  m_element = element_value * euclid_modifier,
  element_value = term + polyrhythm_subseq + polymeter_subseq + polymeter1_subseq,
  elongate = (ws ^ -1 * P("_")) ^ 0,
  polyrhythm_subseq = P("[") * ws ^ -1 * subseq_body * ws ^ -1 * P("]"),
  polymeter_subseq = P("{") * ws ^ -1 * subseq_body * ws ^ -1 * P("}") * polymeter_steps ^ -1,
  polymeter1_subseq = P("<") * ws ^ -1 * subseq_body * ws ^ -1 * P(">"),
  polymeter_steps = P("%") * number,
  subseq_body = sequence * other_subseqs,
  other_subseqs = (ws ^ -1 * P(",") * ws ^ -1 * sequence) ^ 0,
  term = word_with_index + rest + number,
  word_with_index = word * index ^ -1,
  index = P(":") * number,
  euclid_modifier = (P("(") * ws ^ -1 * sequence * ws ^ -1 * P(",") * ws ^ -1 * sequence * euclid_rotation_param ^ -1 * ws ^ -1 * P(")")) ^ 0,
  euclid_rotation_param = ws ^ -1 * P(",") * ws ^ -1 * sequence,
  modifiers = modifier ^ 0,
  modifier = fast + slow + _repeat + degrade + weight,
  fast = P("*") * m_element,
  slow = P("/") * m_element,
  _repeat = (repeat1 + repeatn) ^ 1,
  repeatn = P("!") * -P("!") * pos_integer,
  repeat1 = P("!") * -pos_integer,
  degrade = degrade1 + degraden + degrader,
  degrader = P("?") * -P("?") * pos_real,
  degraden = P("?") * -pos_real * -P("?") * pos_integer,
  degrade1 = P("?") * -pos_integer * -pos_real,
  weight = P("@") * number,
  word = R("az", "AZ") ^ 1 * R("09") ^ -1 * P("'") ^ -1 * chordname ^ -1 * chordmod ^ 0,
  chordname = R("az", "09") ^ 1,
  chordmod = P("'") * ((S("id") + R("09")) + P("o") + R("09")),
  number = real + integer,
  real = integer * P(".") * pos_integer ^ -1,
  pos_real = pos_integer * P(".") * pos_integer ^ -1,
  integer = minus ^ -1 * pos_integer,
  pos_integer = -minus * R("09") ^ 1,
  rest = P("~"),
  minus = P("-"),
  ws = S(" \t") ^ -1
}
grammar = Ct(C(grammar))
local parse
parse = function(string)
  return grammar:match(string)[2]
end
return {
  parse = parse
}
