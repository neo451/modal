import Pattern, fastcat, timecat, pure, silence from require "xi.pattern"
import type, dump, map from require "xi.utils"
require "xi.mini.visitor"
-- type to tag

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
    modifiers = {}
    pat = @eval(node.value)
    values = { { 1, pat, 0 } }
    return values

  modifiers:(node) =>

  number:(node) => pure(node.value)

  -- TODO: if index
  word:(node) =>
    pure(node.value)

  rest:(node) => silence!

export Mini = (source) ->
  ast = Parse_mini(source)
  return Interpreter\eval(ast)
