--- Core pattern representation for strudel
-- @module xi.pattern
import map, filter, reduce, id, flatten, totable, dump, concat, rotate, union, timeToRand, curry, type from require "xi.utils"
import bjork from require "xi.euclid"
import parseChord from require "xi.chords"
import getScale from require "xi.scales"
import Fraction, tofrac, tofloat from require "xi.fraction"
import genericParams, aliasParams from require "xi.control"
import Event, Span, State from require "xi.types"
import visit from require "xi.mini.visitor"
import op from require "fun"
local *
import string_lambda from require("pl.utils")
fun = require "fun"

sin = math.sin
min = math.min
max = math.max
pi = math.pi
floor = math.floor
tinsert = table.insert

C = {}

create = (name) ->
  withVal = (v) -> { [name]: v }
  func = (args) -> reify(args)\fmap(withVal)
  C[name] = func

for name in *genericParams
  param = name[2]
  create param
  if aliasParams[param] != nil
    alias = aliasParams[param]
    C[alias] = C[param]

notemt =
  __add: (other) =>
    { note: @note + other.note }

C.note = (args) ->
  args = reify(args)
  chordToStack = (thing) -> stack(parseChord thing)
  withVal = (v) -> setmetatable { note: v }, notemt
  return reify(args)\fmap(chordToStack)\outerJoin!\fmap(withVal)

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
      tinsert tc_args, { #es * weight, degradeBy(deg_ratio, fastcat(pats)) }
    return timecat tc_args

  random_sequence:(node) =>
    seqs = [@eval(e) for e in *node.elements]
    return randcat seqs

  polyrhythm:(node) =>
    seqs = [@eval(seq) for seq in *node.seqs]
    return polyrhythm seqs

  polymeter:(node) =>
    fast_params = [ Fraction(node.steps, #seq.elements) for seq in *node.seqs]
    return stack([_fast fp, @eval(seq) for _, seq, fp in fun.zip(node.seqs, fast_params)])

  element:(node) =>
    modifiers = [ @eval(mod) for mod in *node.modifiers ]
    pat = @eval(node.value)

    if node.euclid_modifier != nil
      k, n, rotation = @eval node.euclid_modifier
      pat = euclid k, n, rotation, pat

    values = { { 1, pat, 0 } }
    for modifier in *modifiers
      local n_values
      for v in *values
        n_values =  modifier(v)
      values = n_values
    return values

  euclid_modifier:(node) =>
    k = @eval node.k
    n = @eval node.n
    rotation = nil
    if node.rotation != nil then rotation = @eval node.rotation
    else rotation = pure(0)
    return k, n, rotation

  modifier:(node) =>
    switch node.op
      when "fast"
        param = @_sequence_elements({ node.value })
        return (w_p) -> { { w_p[1], fast(param, w_p[2]), w_p[3] } }
      when "slow"
        param = @_sequence_elements({ node.value })
        return (w_p) -> { { w_p[1], slow(param, w_p[2]), w_p[3] } }
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
  -- TODO: not right
  word:(node) =>
    if node.index != 0
      return C.sound(node.value)\combineLeft(C.n(node.index))
    return pure node.value

  rest:(node) => silence!

class Pattern
  new:(query = -> {}) => @query = query

  type: -> "pattern"

  querySpan:(b, e) =>
    span = Span b, e
    state = State span
    @query state

  firstCycle: => @querySpan 0, 1

  __tostring: =>
    func = (event) -> event\show!
    dump map func, @firstCycle!

  show: => @__tostring!

  bindWhole: (choose_whole, func) =>
    query = (_, state) ->
      withWhole = (a, b) ->
        new_whole = choose_whole a.whole, b.whole
        return Event new_whole, b.part, b.value --context?
      match = (a) ->
        events = func(a.value)\querySpan(a.part._begin, a.part._end)
        f = (b) -> withWhole(a, b)
        return map f, events
      events = @query(state)
      return flatten (map ((a) -> match a), events)
    return Pattern query

  outerBind:(func) => @bindWhole ((a) -> a), func

  innerBind:(func) => @bindWhole ((_, b) -> b), func

  outerJoin: => @outerBind id

  innerJoin: => @innerBind id

  filterEvents:(func) =>
    query = (_, state) ->
      events = @query state
      filter func, events
    Pattern query

  filterValues:(condf) =>
    query = (_, state) ->
      events = @query state
      f = (event) ->
        condf event.value
      filter f, events
    Pattern query

  removeNils: => @filterValues (v) -> v ~= nil

  splitQueries: =>
    query = (_, state) ->
      cycles = state.span\spanCycles!
      f = (span) ->
        sub_state = state\setSpan span
        @query sub_state
      flatten map f, cycles
    Pattern query

  withValue:(func) =>
    query = (_, state) ->
      events = @query state
      f = (event) ->
        return event\withValue func
      map f, events
    Pattern query

  fmap:(func) => @withValue(func)

  withQuerySpan:(func) =>
    query = (_, state) ->
      new_state = state\withSpan func
      @query new_state
    Pattern query

  withQueryTime:(func) =>
    @withQuerySpan (span) ->span\withTime func

  withTime:(q_func, e_func) =>
    query = @withQueryTime q_func
    pattern = query\withEventTime e_func
    pattern

  withEventTime:(func) =>
    query = (_, state) ->
      events = @query state
      time_func = (span) -> span\withTime func
      event_func = (event) -> event\withSpan time_func
      map event_func, events
    Pattern query

  onsetsOnly: => @filterEvents (event) -> event\hasOnset!

  appLeft:(pat_val) =>
    query = (_, state) ->
      events = {}
      event_funcs = @query state
      for event_func in *event_funcs
        whole = event_func\wholeOrPart!
        event_vals = pat_val\querySpan(whole._begin, whole._end)
        for event_val in *event_vals
          new_whole = event_func.whole
          new_part = event_func.part\sect event_val.part
          new_context = event_val\combineContext event_func
          if new_part ~= nil
            new_value = event_func.value event_val.value
            tinsert events, Event new_whole, new_part, new_value, new_context
      return events
    Pattern query

  appRight:(pat_val) =>
    query = (_, state) ->
      events = {}
      event_vals = pat_val\query state
      for event_val in *event_vals
        whole = event_val\wholeOrPart!
        event_funcs = @querySpan(whole._begin, whole._end)
        event_val\wholeOrPart!
        for event_func in *event_funcs
          new_whole = event_val.whole
          new_part = event_func.part\sect event_val.part
          new_context = event_val\combineContext event_func
          if new_part ~= nil
            new_value = event_func.value event_val.value
            tinsert events, Event new_whole, new_part, new_value, new_context
        return events
    Pattern query

  combineRight:(other) => @fmap((x) -> (y) -> union(y, x))\appLeft(other)

  combineLeft:(other) => @fmap((x) -> (y) -> union(x, y))\appLeft(other)

  __eq: (other) => @show! == other\show!

  -- metamethods equal to tidal's |x ops
  __mul:(other) => @fmap((x) -> (y) -> y * x)\appLeft(reify(other))

  __div:(other) => @fmap((x) -> (y) -> y / x)\appLeft(reify(other))

  __add:(other) => @fmap((x) -> (y) -> y + x)\appLeft(reify(other))

  __sub:(other) => @fmap((x) -> (y) -> y - x)\appLeft(reify(other))

  __mod:(other) => @fmap((x) -> (y) -> y % x)\appLeft(reify(other))

  __pow:(other) => @fmap((x) -> (y) -> y ^ x)\appLeft(reify(other))

  __concat:(other) => @combineLeft(other)

--- Does absolutely nothing..
-- @return Pattern
-- @usage
-- silence!
silence = -> Pattern!

--- A discrete value that repeats once per cycle.
-- @return Pattern
-- @usage
-- pure('e4')
pure = (value) ->
  query = (state) =>
    cycles = state.span\spanCycles!
    f = (span) ->
      whole = span\wholeCycle span._begin
      Event whole, span, value
    map f, cycles
  Pattern query

--- Turns mini-notation(string) into a pattern
-- @param string
-- @return Pattern
mini = (string) ->
  ast = visit string
  Interpreter\eval ast

--- Turns something into a pattern, unless it's already a pattern
-- @local
-- @return pattern
reify = (thing) ->
  switch type thing
    when "string" then
      if string_lambda thing
        return string_lambda thing
      else
        return mini thing
    when "table" then fastcat thing
    when "pattern" then thing
    else pure thing

-- ** Combining patterns
--- The given items are played at the same time at the same length.
-- @return Pattern
-- @usage
-- stack("g3", "b3", {"e4", "d4"})
stack = (...) ->
  pats = map reify, totable(...)
  query = (state) =>
    span = state.span
    f = (pat) ->
      -- tmp fix? same issue with appLeft why is state mutated? --weird
      pat\querySpan(span._begin, span._end)
    events = map f, pats
    flatten events
  Pattern query

--- Concatenation: combines a list of patterns, switching between them successively, one per cycle. Unlike slowcat, this version will skip cycles.
-- @param items to concatenate
-- @return Pattern
slowcatPrime = (...) ->
  pats = map reify, totable(...)
  query = (state) =>
    len = #pats
    index = state.span._begin\sam!\asFloat! % len + 1
    pat = pats[index]
    pat\query state
  Pattern(query)\splitQueries!

--- Concatenation: combines a list of patterns, switching between them successively, one per cycle:
-- @param items to concatenate
-- @return Pattern
-- @usage
-- slowcat("e5", "b4", {"d5", "c5"})
slowcat = (...) ->
  pats = map reify, totable(...)
  query = (state) =>
    span = state.span
    len = #pats
    index = state.span._begin\sam!\asFloat! % len + 1
    pat = pats[index]
    if not pat then return {}
    offset = span._begin\floor! - (span._begin / len)\floor!
    pat\withEventTime((t) -> t + offset)\query(state\setSpan(span\withTime((t) -> t - offset)))
  Pattern(query)\splitQueries!

--- Like slowcat, but the items are crammed into one cycle.
-- @param items to concatenate
-- @return Pattern
-- @usage
-- fastcat("e5", "b4", {"d5", "c5"})
fastcat = (...) ->
  pats = map reify, totable(...)
  _fast #pats, slowcat(...)

-- Concatsenation: takes a table of time-pat tables, each duration relative to the whole
-- @param table of time-pat tables
-- @return Pattern
-- @usage
-- timecat({{3,"e3"},{1, "g3"}}) // "e3@3 g3"
timecat = (tuples) ->
  pats = {}
  times = map ((x) -> x[1]), tuples
  total = reduce op.add, Fraction(0), times
  accum = Fraction(0)
  for tup in *tuples
    { time, pat } = tup
    b = accum / total
    e = (accum + time) / total
    new_pat = _compress b, e, pat
    tinsert pats, new_pat
    accum = accum + time
  stack(pats)

_fast = (rate, pat) -> return (reify pat)\withTime ((t) -> t * rate), ((t) -> t / rate)

_slow = (factor, pat) -> _fast (1 / factor), pat

_early = (offset, pat) -> (reify pat)\withTime ((t) -> t + offset), ((t) -> t - offset)

_late = (offset, pat) -> _early -offset, pat

_inside = (factor, f, pat) -> _fast factor, f _slow(factor, pat)

_outside = (factor, f, pat) -> _inside(1 / factor, f, pat)

-- ** waveforms
waveform = (func) ->
  query = (state) =>
    return { Event nil, state.span, func state.span\midpoint! }
  Pattern query

steady = (value) -> return Pattern((state) -> { Event nil, state.span, value })

toBipolar = (pat) -> pat\fmap (x) -> x * 2 - 1

fromBipolar = (pat) -> pat\fmap((x) -> (x + 1) / 2)

sine2 = waveform (t) -> sin(t\asFloat! * pi * 2)

sine = fromBipolar sine2

cosine2 = _late 1/4, sine2

cosine = fromBipolar cosine2

square = waveform (t) -> floor ( t * 2 ) % 2

square2 = toBipolar square

isaw = waveform (t) -> -(t % 1) + 1
isaw2 = toBipolar isaw

saw = waveform (t) -> t % 1
saw2 = toBipolar saw

tri = fastcat isaw, saw
tri2 = fastcat isaw2, saw2

time = waveform id

-- ** randoms
rand = waveform timeToRand

_irand = (i) -> rand\fmap (x) -> floor x * i

irand = (ipat) -> reify(ipat)\fmap(_irand)\innerJoin!

run = (n) -> fastcat [i for i in range(0, n - 1)]

scan = (n) -> slowcat [run(i) for i in range(n)]

_chooseWith = (pat, ...) ->
  vals = map reify, totable(...)
  if #vals == 0 then return silence!
  return range(1, #vals + 1, pat)\fmap(
    (i) ->
      key = min(max(floor(i), 0), #vals)
      return vals[key]
  )

chooseWith = (pat, ...) ->
  _chooseWith(pat, ...)\outerJoin!

choose = (...) -> chooseWith rand, ...

chooseCycles = (...) -> segment 1, choose(...)

randcat = (...) -> chooseCycles(...)

polyrhythm = (...) -> stack(...)

_patternify = (func) ->
  patterned = (apat, pat) ->
    -- tmp hack for partial application, but apats are not patternified
    if pat == nil then return curry(func, 2)(apat)
    apat = reify apat
    mapFn = (a) -> func(a, pat)
    return apat\fmap(mapFn)\innerJoin!
  return patterned

_patternify_p_p = (func) ->
  patterned = (apat, bpat, pat) ->
    if pat == nil then return curry(func, 3)(apat)(bpat)
    apat, bpat, pat = reify(apat), reify(bpat), reify(pat)
    mapFn = (a, b) -> func a, b, pat
    mapFn = curry mapFn, 2
    patOfPats = apat\fmap(mapFn)\appLeft(bpat)
    return patOfPats\innerJoin!
  return patterned

_patternify_p_p_p = (func) ->
  patterned = (apat, bpat, cpat, pat) ->
    if pat == nil then return curry(func, 4)(apat)(bpat)(cpat)
    apat, bpat, cpat, pat = reify(apat), reify(bpat), reify(cpat), reify(pat)
    mapFn = (a, b, c) -> func a, b, c, pat
    mapFn = curry mapFn, 3
    patOfPats = apat\fmap(mapFn)\appLeft(bpat)\appLeft(cpat)
    return patOfPats\innerJoin!
  return patterned

_off = (time_pat, f, pat) -> stack(pat, f late(time_pat, pat))

struct = (boolpat, pat) ->
  pat, boolpat = reify(pat), reify(boolpat)
  boolpat\fmap((b) -> (val) -> if b return val else nil)\appLeft(pat)\removeNils!

_euclid = (n, k, offset, pat) -> struct bjork(n, k, offset), reify(pat)

_juxBy = (by, f, pat) ->
  by = by / 2
  elem_or = (dict, key, default) ->
    if dict[key] != nil then return dict[key]
    return default
  left = pat\fmap((valmap) -> union { pan: elem_or(valmap, "pan", 0.5) - by }, valmap)
  right = pat\fmap((valmap) -> union { pan: elem_or(valmap, "pan", 0.5) + by }, valmap)
  return stack left, f right

_jux = (f, pat) -> _juxBy(0.5, f, pat)

superimpose = (f, pat) -> stack(pat, f(pat))

layer = (table, pat) -> stack [f reify pat for f in *table]

rev = (pat) ->
  pat = reify pat
  query = (_, state) ->
    span = state.span
    cycle = span._begin\sam!
    nextCycle = span._begin\nextSam!
    reflect = (to_reflect) ->
      reflected = to_reflect\withTime((time) -> cycle + ( nextCycle - time))
      tmp = reflected._begin
      reflected._begin = reflected._end
      reflected._end = tmp
      return reflected
    events = pat\query state\setSpan reflect(span)
    map ((event) -> event\withSpan reflect), events
  Pattern query

palindrome = (pat) -> slowcat pat, rev pat

_iter = (n, pat) -> slowcat [_early Fraction(i - 1, n), reify pat for i in fun.range(n)]

_reviter = (n, pat) -> slowcat [_late Fraction(i - 1, n), reify pat for i in fun.range(n)]

_segment = (n, pat) -> fast(n, pure(id))\appLeft pat

_range = (min, max, pat) -> pat\fmap((x) -> x * (max - min) + min)

_fastgap = (factor, pat) ->
  pat = reify pat
  factor = tofrac(factor)
  if factor <= Fraction(0) then return silence!
  factor = factor\max(1)
  mungeQuery = (t) ->
    t\sam! + ((t - t\sam!) * factor)\min(1)
  eventSpanFunc = (span) ->
    b = span._begin\sam! + (span._begin - span._begin\sam!) / factor
    e = span._begin\sam! + (span._end - span._begin\sam!) / factor
    Span b, e
  query = (_, state) ->
    span = state.span
    new_span = Span mungeQuery(span._begin), mungeQuery(span._end)
    if new_span._begin == new_span._begin\nextSam!
      return {}
    new_state = State new_span
    events = pat\query new_state
    f = (event) ->
      event\withSpan eventSpanFunc
    map f, events
  Pattern(query)\splitQueries!

_compress = (b, e, pat) ->
  pat = reify pat
  b, e = tofrac(b), tofrac(e)
  if b > e or e > Fraction(1) or b > Fraction(1) or b < Fraction(0) or e < Fraction(0)
    return silence!
  fasted = _fastgap (Fraction(1) / (e - b)), pat
  _late b, fasted

_degradeByWith = (prand, by, pat) ->
  pat = reify pat
  if type(by) == "fraction" then by = by\asFloat!
  f = (v) -> v > by
  return pat\fmap((val) -> (_) -> val)\appLeft(prand\filterValues(f))

_degradeBy = (by, pat) -> _degradeByWith(rand, by, pat)

_undegradeBy = (by, pat) -> _degradeByWith(rand\fmap((r) -> 1 - r), by, pat)

degrade = (pat) -> _degradeBy(0.5, pat)

undegrade = (pat) -> _undegradeBy(0.5, pat)

-- ** higher order functions
sometimesBy = (by, func, pat) ->
  (reify(by)\fmap (by) ->  stack _degradeBy(by, pat), func _undegradeBy(1 - by, pat))\innerJoin!

sometimes = (func, pat) -> sometimesBy(0.5, func, pat)

_when = (bool, f, pat) -> bool and f(pat) or pat

_firstOf = (n, f, pat) ->
  boolpat = fastcat concat { true }, [false for i = 1, n - 1]
  when_ boolpat, f, pat

_lastOf = (n, f, pat) ->
  boolpat = fastcat concat [false for i = 1, n - 1], {true}
  when_ boolpat, f, pat

-- ** music theory
_scale = (name, pat) ->
  pat = reify pat
  toScale = (v) -> getScale(name, v)
  pat\fmap(toScale)


-- ** composing
-- TODO: squeezeJoin!
-- _inhabit = (tablepat, index) ->
--   return fastcat tablepat[index]

scale = _patternify _scale
fastgap = _patternify _fastgap
degradeBy = _patternify _degradeBy
undegradeBy = _patternify _undegradeBy
segment = _patternify _segment
fast = _patternify _fast
slow = _patternify _slow
iter = _patternify _iter
reviter = _patternify _reviter
early = _patternify _early
late = _patternify _late
inside = _patternify_p_p _inside
outside = _patternify_p_p _outside
when_ = _patternify_p_p _when -- when is a reserved keyword ...
firstOf = _patternify_p_p _firstOf
lastOf = _patternify_p_p _lastOf
every = firstOf
off = _patternify_p_p _off
range = _patternify_p_p _range
compress = _patternify_p_p _compress
euclid = _patternify_p_p_p _euclid
jux = _patternify _jux
juxBy = _patternify_p_p _juxBy

apply = (x, pat) -> pat .. x

-- TODO: wchoose, tests for the new functions

sl = string_lambda

return {
  :when_
  :sl
  :C
  :Pattern
  :apply
  :id
  :pure
  :silence
  :mini
  :reify
  :run
  :scan
  :fastcat
  :slowcat
  :timecat
  :randcat
  :euclid
  :stack
  :layer
  :superimpose
  :struct
  :jux
  :juxBy
  :inside
  :outside
  :firstOf
  :lastOf
  :every
  :rev
  :off
  :every
  :fast
  :slow
  :early
  :late
  :fastgap
  :compress
  :degrade
  :degradeBy
  :undegradeBy
  :undegrade
  :sometimes
  :iter
  :reviter
  :scale
  :sine, :sine2, :square, :square2, :saw, :saw2, :isaw, :isaw2, :tri, :tri2, :rand, :irand
}
