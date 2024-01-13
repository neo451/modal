require "xi.mini.grammar"

-- TODO: type to tag?
class Visitor
  visit: (node) =>
    type = node[1]
    method = @[type]
    if node[3] ~= nil
      children = {}
      for i = 3, #node
        children[#children + 1] = node[i]
      for i, subnode in pairs(children)
        children[i] = @visit(subnode)
      method(self, node, children)
    else
      method(self, node, "")

  root:(_, children) => children[1]

  sequence:(_, children) =>
    { group, other_groups, other_seqs } = children

    if other_groups != ""
      elements = {
        {
          type: "element",
          value: { type: "polyrhythm", seqs: { group } },
          modifiers: {}
        },
      }
      for item in *other_groups
        elements[#elements + 1] = {
          type: "element",
          value: {
            type: "polyrhythm",
            seqs: { item },
          },
          modifiers: {},
        }
      group = { type: "sequence", elements: elements }

    if other_seqs != "" then
      n_seq = { group }
      for item in *other_seqs
        n_seq[#n_seq + 1] = item
      return { type: "random_sequence", elements: n_seq }
    group

  group:(_, children) =>
    { elements, other_elements } = children
    n_elements = { elements }
    if other_elements != ""
      for item in *other_elements
        n_elements[#n_elements + 1] = item
    { type: "sequence", elements: n_elements }

  other:(_, children) => children or "" -- TODO: better way to do this?

  element:(_, children) =>
    { value, eculid_modifier, modifiers, elongate } = children
    weight_mods, n_mods = {}, {}
    for mod in *modifiers
      if mod.op == "weight"
        table.insert weight_mods, mod
      else
        table.insert n_mods, mod
    weight_mod = weight_mods[1] or { type: "modifier", op: "weight", value: elongate }
    if weight_mod.value ~= 1
      table.insert n_mods, weight_mod
    { type: "element", value: value, modifiers: n_mods, euclid_modifier: eculid_modifier }

  polyrhythm_subseq:(_, children) =>
    { type: "polyrhythm", seqs: children[1] }

  polymeter_subseq:(_, children) =>
    { seqs, steps } = children
    { type: "polymeter", seqs: seqs, steps: steps or 1 }

  polymeter_steps:(_, children) => children[1]

  polymeter1_subseq:(_, children) =>
    { type: "polymeter", seqs: children[1], steps: 1 }

  subseq_body:(_, children) =>
    { seq, other_seqs } = children
    n_subseqs = { seq }
    if other_seqs != ""
      for item in *other_seqs
        table.insert n_subseqs, item
    return n_subseqs

  elongate:(node, _) => #node[2] / 2 + 1

  element_value:(_, children) => children[1]

  term:(_, children) =>
    if type(children[1]) == "number" then
      return { type: "number", value: children[1] }
    children[1]

  rest:(_, _) => { type: "rest" }

  word_with_index:(_, children) =>
    { word, index } = children
    { type: "word", value: word, index: index or 0 }

  index:(_, children) => children[1]

  euclid_modifier:(_, children) =>
    if children == "" then
      return
    { k, n, rotation } = children
    if k ~= nil and n ~= nil
      return { type: "euclid_modifier", k: k, n: n, rotation: rotation }
	
  euclid_rotation_param: (_, children) => children[1]

  modifiers:(_, children) =>
    if children == ""
      return {}
    mods, degrade_mods, weight_mods = {}, {}, {}
    count_deg_mods, value_deg_mods = {}, {}
    for mod in *children
      if mod.op == "degrade"
        table.insert degrade_mods, mod
      elseif mod.op == "weight"
        table.insert weight_mods, mod
      else
        table.insert mods, mod
    if #degrade_mods > 0
      for mod in *degrade_mods
        if mod.value.op == "count"
          table.insert count_deg_mods, mod
        else
          table.insert value_deg_mods, mod
    if #value_deg_mods > 0
      table.insert mods, value_deg_mods[#value_deg_mods]
    elseif #count_deg_mods > 0
      sum = 0
      for mod in *count_deg_mods
        sum = sum + mod.value.value
      table.insert mods, { type: "modifier", op: "degrade", value: { type: "degrade_arg", op: "count", value: sum } }
    table.insert mods, weight_mods[#weight_mods]
    return mods

  modifier:(_, children) => children[1]

  fast:(_, children) => { type: "modifier", op: "fast", value: children[1] }

  slow:(_, children) => { type: "modifier", op: "slow", value: children[1] }

  _repeat:(_, children) =>
    sum = 0
    for _, v in pairs(children) do
      sum = sum + v
    { type: "modifier", op: "repeat", count: sum }

  repeatn:(_, children) => children[1]

  repeat1:(_, _) => 1

  degrade:(_, children) => { type: "modifier", op: "degrade", value: children[1] }

  degrader:(_, children) => { type: "degrade_arg", op: "value", value: children[1] }

  degraden:(_, children) => { type: "degrade_arg", op: "count", value: children[1] }

  degrade1:(_, _) => { type: "degrade_arg", op: "count", value: 1 }

  weight:(_, children) => { type: "modifier", op: "weight", value: children[1] }

  word:(node, _) => node[2]

  number:(_, children) => children[1]

  integer:(node, _) => tonumber node[2]

  real:(node, _) => tonumber node[2]

  pos_integer:(node, _) => tonumber node[2]

  pos_real:(node, _) => tonumber node[2]

return { Visitor: Visitor }
