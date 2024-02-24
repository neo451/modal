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
local slice = token("slice")
local sub_cycle = token("sub_cycle")
local polymeter = token("polymeter")
local slow_sequence = token("slow_sequence")
local polymeter_steps = token("polymeter_steps")
local subseq_body = token("subseq_body")
local step = token("step")
local slice_with_ops = token("slice_with_ops")
local ops = token("ops")
local op = token("op")
local fast = token("fast")
local slow = token("slow")
local r1 = token("r1")
local rn = token("rn")
local replicate = token("replicate")
local degrade = token("degrade")
local degrade1 = token("degrade1")
local degrader = token("degrader")
local degraden = token("degraden")
local weight = token("weight")
local euclid = token("euclid")
local euclid_rotation = token("euclid_rotation")
local tail = token("tail")
local range = token("range")
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
  group = slice_with_ops * other_elements,
  other_elements = (ws * -P(".") * slice_with_ops) ^ 0,
  slice_with_ops = slice * op ^ 0,
  slice = step + sub_cycle + polymeter + slow_sequence,
  sub_cycle = P("[") * ws ^ -1 * subseq_body * ws ^ -1 * P("]"),
  polymeter = P("{") * ws ^ -1 * subseq_body * ws ^ -1 * P("}") * polymeter_steps ^ -1,
  slow_sequence = P("<") * ws ^ -1 * subseq_body * ws ^ -1 * P(">"),
  polymeter_steps = P("%") * slice,
  subseq_body = sequence * other_subseqs,
  other_subseqs = (ws ^ -1 * P(",") * ws ^ -1 * sequence) ^ 0,
  step = word + rest + number,
  op = fast + slow + replicate + degrade + weight + euclid + tail + range,
  fast = P("*") * slice,
  slow = P("/") * slice,
  replicate = (r1 + rn) ^ 1,
  rn = P("!") * -P("!") * pos_integer,
  r1 = P("!") * -pos_integer,
  degrade = degrade1 + degraden + degrader,
  degrader = P("?") * -P("?") * pos_real,
  degraden = P("?") * -pos_real * -P("?") * pos_integer,
  degrade1 = P("?") * -pos_integer * -pos_real,
  elongate = (ws ^ -1 * P("_")) ^ 0,
  weight = P("@") * number,
  euclid = P("(") * ws ^ -1 * sequence * ws ^ -1 * P(",") * ws ^ -1 * sequence * euclid_rotation ^ -1 * ws ^ -1 * P(")"),
  euclid_rotation = ws ^ -1 * P(",") * ws ^ -1 * sequence,
  tail = P(":") * number,
  range = P(":") * number,
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
  ws = S(" \n\r\t\u00A0") ^ -1,
  comma = ws * P(",") * ws,
  pipe = ws * P("|") * ws,
  dot = ws * P(".") * ws,
  quote = P("'") + P('"')
}
grammar = Ct(C(grammar))
local parse
parse = function(string)
  return grammar:match(string)[2]
end
p(parse("45"))
return {
  parse = parse
}
