

export class Visitor
  visit: (node) =>
    type = node[1]
    method = @[type]
    if node[3] ~= nil
      children = {}
      for i = 3, #node
        children[#children + 1] = node[i]
      for i, subnode in pairs(children)
        children[i] = self:visit(subnode)
      method(self, node, children)
    else
      method(self, node, "")

  root:(_, children) => children[1]

  sequence:(_, children) =>
    { group, other_groups, other_seqs } = children

    if other_groups != ""
      elements: {
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
      n_seq: { group }
      for item in *other_seqs
        n_seq[#n_seq + 1] = item
      { type: "random_sequence", elements: n_seq }
    group

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
