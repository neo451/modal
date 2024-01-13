import Pattern, fastcat, timecat, pure, silence from require "xi.pattern"
import type, dump, map, flatten, id from require "xi.utils"
require "xi.mini.visitor"

class Interpreter
  eval:(node) =>
    tag = node.type
    method = @[tag]
    return method(@, node)

  sequence:(node) =>
    @_sequence_elements(node.elements)

  _sequence_elements:(elements) =>
    elements = [@eval(e) for e in *elements]
    tc_args = {}
    for es in *elements
      weight = es[1][1] or 1
      deg_ratio = es[1][3] or 1 --??
      pats = [e[2] for e in *es]
      table.insert tc_args, { #es * weight, fastcat(pats) }
    return timecat tc_args

  element:(node) =>
    modifiers = [ @eval(mod) for mod in *node.modifiers ]
    pat = @eval(node.value)
    values = { { 1, pat, 0 } }

    for modifier in *modifiers
      -- values = flatten [ modifier(v) for v in *values ]
      values = modifier(values[1])

    return values

  modifier:(node) =>
    -- param = @_sequence_elements({ node.value })
    switch node.op
      when "fast"
        param = node.value.value.value --HACK: tmp fix, no patternify at the moment
        return (w_p) -> { { w_p[1], w_p[2]\_fast(param), w_p[3] } }
      when "slow"
        param = node.value.value.value --HACK: tmp fix, no patternify at the moment
        return (w_p) -> { { w_p[1], w_p[2]\_slow(param), w_p[3] } }
      when "repeat"
        return (w_p) -> [ w_p for i = 1, node.count + 1 ]
      when "weight"
        return (w_p) -> { { node.value, w_p[2], w_p[3] }}
    return id --?


  number:(node) => pure node.value

  -- TODO: if index
  word:(node) =>
    pure node.value

  rest:(node) => silence!


return { Interpreter: Interpreter }
