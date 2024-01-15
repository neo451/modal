import Pattern, fastcat, timecat, pure, silence, randcat, polyrhythm, stack from require "xi.pattern"
import type, dump, map, flatten, id from require "xi.utils"
import Fraction from require "xi.fraction"
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
      deg_ratio = es[1][3] or 0
      pats = [e[2] for e in *es]
      table.insert tc_args, { #es * weight, fastcat(pats)\degrade_by(deg_ratio) }
    return timecat tc_args

  random_sequence:(node) =>
    seqs = [@eval(e) for e in *node.elements]
    return randcat seqs

  polyrhythm:(node) =>
    seqs = [@eval(seq) for seq in *node.seqs]
    return polyrhythm seqs

  polymeter:(node) =>
    fast_params = [ Fraction(node.steps, #seq.elements) for seq in *node.seqs]
    p fast_params
    for seq in *node.seqs
      print dump @eval(seq)\fast(fast_params[1])\querySpan(0, 2)
    return stack([@eval(seq)\fast(fp) for _, seq, fp in zip(node.seqs, fast_params)])

  element:(node) =>
    modifiers = [ @eval(mod) for mod in *node.modifiers]
    pat = @eval(node.value)
    values = { { 1, pat, 0 } }

    for modifier in *modifiers
      n_values = nil
      for v in *values
        n_values =  modifier(v)
      values = n_values
    return values

  modifier:(node) =>
    switch node.op
      when "fast"
        param = @_sequence_elements({ node.value })
        return (w_p) -> { { w_p[1], w_p[2]\fast(param), w_p[3] } }
      when "slow"
        param = @_sequence_elements({ node.value })
        return (w_p) -> { { w_p[1], w_p[2]\slow(param), w_p[3] } }
      when "repeat"
        return (w_p) -> [ w_p for i = 1, node.count + 1 ]
      when "weight"
        return (w_p) -> { { node.value, w_p[2], w_p[3] } }
      when "degrade"
        arg = node.value
        switch arg.op
          when "count"
            return (w_p) -> { { w_p[1], w_p[2], Fraction(arg.value, arg.value + 1) } }
          when "value"
            return (w_p) -> { { w_p[1], w_p[2], arg.value } }
    return (w_p) -> { { w_p[1], w_p[2], w_p[3] } }

  number:(node) => pure node.value

  -- TODO: if index
  word:(node) =>
    pure node.value

  rest:(node) => silence!

import Parse from require "xi.mini.grammar"
import Visitor from require "xi.mini.visitor"

parse = (code) ->
  raw_ast = Parse code
  return Visitor\visit raw_ast

mini = (source) ->
  ast = parse source
  return Interpreter\eval ast

return { Interpreter: Interpreter }
