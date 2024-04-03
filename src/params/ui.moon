import stack, slow, pure, fastcat, _patternify, _patternify_p_p, _patternify_p_p_p from require "modal.pattern"
import Fraction, tofrac, tofloat from require "modal.fraction"
import map, filter, string_lambda, reduce, id, flatten, totable, dump, concat, rotate, union, timeToRand, curry, type from require "modal.utils"
import Event, Span, State from require "modal.types"
P = require"modal.params"
local *

_juxBy = (by, f, pat) ->
  by = by / 2
  elem_or = (dict, key, default) ->
    if dict[key] != nil then return dict[key]
    return default
  left = pat\fmap((valmap) -> union { pan: elem_or(valmap, "pan", 0.5) - by }, valmap)
  right = pat\fmap((valmap) -> union { pan: elem_or(valmap, "pan", 0.5) + by }, valmap)
  return stack left, f right
juxBy = _patternify_p_p _juxBy

_jux = (f, pat) -> _juxBy(0.5, f, pat)
jux = _patternify _jux

-- @section sampling
_striate = (n, pat) ->
  ranges = [ { begin: i / n, end: (i + 1) / n } for i = 0, n - 1 ]
  merge_sample = (range) ->
    f = (v) -> union range, { sound: v.sound }
    pat\fmap f
  return fastcat [merge_sample(r) for r in *ranges ]

striate = _patternify _striate

_chop = (n, pat) ->
  ranges = [ { begin: i / n, end: (i + 1) / n } for i = 0, n - 1 ]
  func = (o) ->
    f = (slice) -> union slice, o
    fastcat map f, ranges
  return pat\squeezeBind func

chop = _patternify _chop
-- TODO
slice = (npat, ipat, opat) ->
  npat\innerBind (n) ->
    ipat\outerBind (i) ->
      opat\outerBind (o) ->
        -- if type(o) != table then o = { sound: o }
        begin = if type(n) == table then begin = n[i] else begin = i / n
        _end = if type(n) == table then _end = n[i + 1] else _end = (i + 1) / n
        return pure union o, { begin: begin, end: _end, _slices: n }

-- TODO
splice = (npat, ipat, opat) ->
  sliced = slice npat, ipat, opat
  sliced\withEvent (event) ->
    event\withValue (value) ->
      new_attri = {
        speed: tofloat(tofrac(1) / tofrac(value._slices) / event.whole\duration!) * (value.speed or 1),
          unit: "c"
      }
      return union new_attri, value

_loopAt = (factor, pat) ->
  pat = pat .. P.speed(1 / factor) .. P.unit("c")
  slow factor, pat

loopAt = _patternify _loopAt

fit = (pat) ->
  pat\withEvent (event) ->
    event\withValue (value) ->
      union value, { speed: tofrac(1) / event.whole\duration!, unit: "c" }

_legato = (factor, pat) ->
  factor = tofrac factor
  pat\withEventSpan (span) ->
    Span span._begin, (span._begin + span\duration! * factor)

legato = _patternify _legato

return {
  :jux, :juxBy
  :striate, :chop
  :slice, :splice
  :loopAt
  :legato
}
