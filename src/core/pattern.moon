--- Core pattern representation
-- @module xi.pattern
local *
import map, filter, reduce, id, flatten, totable, dump, concat, rotate, union, timeToRand, curry, type from require "xi.utils"
import bjork from require "xi.euclid"
import parseChord from require "xi.chords"
import getScale from require "xi.scales"
import Fraction, tofrac, tofloat from require "xi.fraction"
import genericParams, aliasParams from require "xi.control"
import Event, Span, State from require "xi.types"
import parse from require "xi.mini.grammar"
import op from require "fun"
import string_lambda from require("pl.utils")
fun = require "fun"

-- applyOpts = (parent, enter) ->
--   (pat, i) ->
--     ast = parent.source[i]
--     options = ast.options
--     ops = options.ops -- ?
--
--     if ops
--       for op in *ops
--         switch op.type
--           when "stretch"
--             { type, amount } op.arguments
--             if type = "fast"
--               pat = fast enter(amount), reify pat -- recursive??
--             elseif type = "slow"
--               pat = slow enter(amount), pat
--             else
--               print("mini: stretch: type must be one of ${legalTypes.join('|')} but got ${type}")

resolveReplications = id
applyOptions = id

patternifyAST = (ast) ->
  enter = (node) -> patternifyAST(node)
  switch ast.type
    when "pattern"
      resolveReplications ast
      children = ast.source
      children = map enter, children
      return fastcat children
    when "element"
      return enter(ast.source)
    when "atom"
      if ast.source == "~" then return silence!
      value = ast.source
      if (tonumber value)
        value = tonumber value

      return pure value

--- Turns mini-notation(string) into a pattern
-- @param string
-- @return Pattern
mini = (code) ->
  ast = parse code
  patternifyAST(ast)

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


class Pattern
  new:(query = -> {}) => @query = query

  type: -> "pattern"

  querySpan:(b, e) =>
    span = Span b, e
    state = State span
    @query state

  __call:(b, e) => @querySpan(b, e)

  firstCycle: => @ 0, 1

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
        events = func(a.value)(a.part._begin, a.part._end)
        f = (b) -> withWhole(a, b)
        return map f, events
      events = @query(state)
      return flatten (map ((a) -> match a), events)
    return Pattern query

  outerBind:(func) => @bindWhole ((a) -> a), func

  innerBind:(func) => @bindWhole ((_, b) -> b), func

  outerJoin: => @outerBind id

  innerJoin: => @innerBind id

  squeezeJoin: =>
    query = (_, state) ->
      events = @discreteOnly!\query(state)
      flatEvent = (outerEvent) ->
        innerPat = focusSpan outerEvent\wholeOrPart!, outerEvent.value
        innerEvents = innerPat\query(State outerEvent.part)
        munge = (outer, inner) ->
          whole = nil
          if inner.whole and outer.whole
            whole = inner.whole\sect(outer.whole)
            if not whole
              return nil
          part = inner.part\sect(outer.part)
          if not part then return nil
          -- context = inner\combineContext(outer)
          return Event whole, part, inner.value -- context
        f = (innerEvent) -> munge(outerEvent, innerEvent)
        return map f, innerEvents
      result = flatten map flatEvent, events
      -- remove nils?
      return filter ((x) -> x), result
    return Pattern query

  squeezeBind:(func) => @fmap(func)\squeezeJoin!

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

  withTime:(qf, ef) =>
    query = @withQueryTime qf
    pattern = query\withEventTime ef
    pattern

  withEvents:(func) => Pattern (_, state) -> func @query(state)

  withEvent:(func) => @withEvents (events) -> map func, events

  withEventSpan:(func) =>
    query = (_, state) ->
      events = @query state
      f = (event) -> event\withSpan func
      map f, events
    Pattern query

  withEventTime:(func) =>
    query = (_, state) ->
      events = @query state
      time_func = (span) -> span\withTime func
      event_func = (event) -> event\withSpan time_func
      map event_func, events
    Pattern query

  onsetsOnly: => @filterEvents (event) -> event\hasOnset!

  discreteOnly: => @filterEvents (event) -> event.whole

  appLeft:(pat_val) =>
    query = (_, state) ->
      events = {}
      event_funcs = @query state
      for event_func in *event_funcs
        whole = event_func\wholeOrPart!
        event_vals = pat_val(whole._begin, whole._end)
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
        event_funcs = @(whole._begin, whole._end)
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
  __mul:(other) => @fmap((x) -> (y) -> y * x)\appLeft reify(other)

  __div:(other) => @fmap((x) -> (y) -> y / x)\appLeft reify(other)

  __add:(other) => @fmap((x) -> (y) -> y + x)\appLeft reify(other)

  __sub:(other) => @fmap((x) -> (y) -> y - x)\appLeft reify(other)

  __mod:(other) => @fmap((x) -> (y) -> y % x)\appLeft reify(other)

  __pow:(other) => @fmap((x) -> (y) -> y ^ x)\appLeft reify(other)

  __concat:(other) => @combineLeft(other)

--- Does absolutely nothing..
-- @usage
-- silence! \\ "~"
silence = -> Pattern!

--- A discrete value that repeats once per cycle.
-- @return Pattern
-- @usage
-- pure('e4') \\ "e4"
pure = (value) ->
  query = (state) =>
    cycles = state.span\spanCycles!
    f = (span) ->
      whole = span\wholeCycle span._begin
      Event whole, span, value
    map f, cycles
  Pattern query


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
    when "pattern" then thing
    else pure thing

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

-- @section Combining patterns
--- The given items are played at the same time at the same length.
-- @usage
-- stack("g3", "b3", "e4", "d4") // "[g3, b3, e4, d4]"
stack = (...) ->
  pats = map reify, totable(...)
  query = (state) =>
    span = state.span
    f = (pat) ->
      -- tmp fix? same issue with appLeft why is state mutated? --weird
      pat(span._begin, span._end)
    events = map f, pats
    flatten events
  Pattern query

--- Concatenation: combines a list of patterns, switching between them successively, one per cycle. Unlike slowcat, this version will skip cycles.
-- @local
slowcatPrime = (...) ->
  pats = map reify, totable(...)
  query = (state) =>
    len = #pats
    index = state.span._begin\sam!\asFloat! % len + 1
    pat = pats[index]
    pat\query state
  Pattern(query)\splitQueries!

--- Concatenation: combines a list of patterns, switching between them successively, one per cycle:
-- @usage
-- slowcat("e5", "b4", "d5", "c5") // "<e5 b4 d5 c5>"
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
-- @usage
-- fastcat("e5", "b4", "d5", "c5") // "e5 b4 d5 c5"
fastcat = (...) ->
  pats = map reify, totable(...)
  _fast #pats, slowcat(...)

--- Concatsenation: takes a table of time-pat tables, each duration relative to the whole
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

-- @section manipulating time

_cpm = (cpm, pat) ->
  (reify pat)\_fast(cpm / 60)
--- Plays the pattern at the given cycles per minute.
-- @usage
-- cpm 90, s"<bd sd>,hh*2" // = 90 bpm
cpm = _patternify _cpm

_fast = (factor, pat) ->
  pat\withTime ((t) -> t * factor), ((t) -> t / factor)
--- Speed up a pattern by the given factor. Used by "*" in mini notation.
-- @usage
-- fast(2, s"bd") // s"bd*2"
fast = _patternify _fast

_slow = (factor, pat) -> _fast (1 / factor), pat
--- Slow down a pattern over the given number of cycles. Like the "/" operator in mini notation.
-- @usage
-- slow(2, s("<bd sd> hh")) // s"[<bd sd> hh]/2"
slow = _patternify _slow

_early = (offset, pat) -> pat\withTime ((t) -> t + offset), ((t) -> t - offset)
--- Nudge a pattern to start earlier in time. Equivalent of Tidal's <~ operator
-- @usage
-- s(stack("bd ~", early(0.1, "hh ~")))
early = _patternify _early

_late = (offset, pat) -> _early -offset, pat
--- Nudge a pattern to start later in time. Equivalent of Tidal's ~> operator
-- @usage
-- s(stack("bd ~", late(0.1, "hh ~")))
late = _patternify _late

_inside = (factor, f, pat) -> _fast factor, f _slow(factor, pat)
--- Carries out an operation 'inside' a cycle.
-- @usage
-- inside(4, rev, "0 1 2 3 4 3 2 1") // fast(4, rev(slow(4, "0 1 2 3 4 3 2 1")))
inside = _patternify_p_p _inside

_outside = (factor, f, pat) -> _inside(1 / factor, f, pat)
--- Carries out an operation 'outside' a cycle.
-- @usage
-- outside(4, rev, "<[0 1] 2 [3 4] 5>") // slow(4, rev(fast(4, "<[0 1] 2 [3 4] 5>")))
outside = _patternify_p_p _outside

_ply = (factor, pat) ->
  pat = pure _fast factor, pat
  pat\squeezeJoin!

ply = _patternify _ply

_fastgap = (factor, pat) ->
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

--- speeds up a pattern like fast, but rather than it playing multiple times as fast would it instead leaves a gap in the remaining space of the cycle. For example, the following will play the sound pattern "bd sn" only once but compressed into the first half of the cycle, i.e. twice as fast.
-- @usage
-- fastgap(2, s("bd sd"))
fastgap = _patternify _fastgap

_compress = (b, e, pat) ->
  b, e = tofrac(b), tofrac(e)
  if b > e or e > Fraction(1) or b > Fraction(1) or b < Fraction(0) or e < Fraction(0)
    return silence!
  fasted = _fastgap (Fraction(1) / (e - b)), pat
  _late b, fasted

--- Compress each cycle into the given timespan, leaving a gap
-- @usage
-- compress(.25,.75, s"bd sd") // s"~ bd sd ~"
compress = _patternify_p_p _compress

_focus = (b, e, pat) ->
  b, e = tofrac(b), tofrac(e)
  fasted = _fast (Fraction(1) / (e - b)), pat
  _late Span\cyclePos(b), fasted

focusSpan = (span, pat) -> _focus span._begin, span._end, reify pat

-- Similar to compress, but doesn't leave gaps, and the 'focus' can be bigger than a cycle
-- @usage
-- focus(1/4, 3/4, s("bd hh sd hh"))
focus = _patternify_p_p _focus

_zoom = (s, e, pat) ->
  s, e = tofrac(s), tofrac(e)
  dur = e - s
  qf = (span) -> span\withCycle (t) -> t * dur + s
  ef = (span) -> span\withCycle (t) -> (t - s) / dur
  return pat\withQuerySpan(qf)\withEventSpan(ef)\splitQueries!

--- Plays a portion of a pattern, specified by the beginning and end of a time span. The new resulting pattern is played over the time period of the original pattern
-- @usage
-- zoom(0.25, 0.75, s"bd*2 hh*3 [sd bd]*2 perc") // s("hh*3 [sd bd]*2")
zoom = _patternify_p_p _zoom

-- @section waveforms
run = (n) -> fastcat [i for i in range(0, n - 1)]

scan = (n) -> slowcat [run(i) for i in range(n)]

waveform = (func) ->
  query = (state) =>
    return { Event nil, state.span, func state.span\midpoint! }
  Pattern query

steady = (value) -> return Pattern((state) -> { Event nil, state.span, value })

toBipolar = (pat) -> pat\fmap (x) -> x * 2 - 1

fromBipolar = (pat) -> pat\fmap (x) -> (x + 1) / 2

sine2 = waveform (t) -> sin(t\asFloat! * pi * 2)

sine = fromBipolar sine2

cosine2 = _late 1/4, sine2

cosine = fromBipolar cosine2

square = waveform (t) -> floor (t * 2) % 2

square2 = toBipolar square

isaw = waveform (t) -> -(t % 1) + 1

isaw2 = toBipolar isaw

saw = waveform (t) -> t % 1

saw2 = toBipolar saw

tri = fastcat isaw, saw

tri2 = fastcat isaw2, saw2

time = waveform id

-- @section randoms
rand = waveform timeToRand

_irand = (i) -> rand\fmap (x) -> floor x * i

irand = (ipat) -> reify(ipat)\fmap(_irand)\innerJoin!

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

_degradeByWith = (prand, by, pat) ->
  pat = reify pat
  if type(by) == "fraction" then by = by\asFloat!
  f = (v) -> v > by
  return pat\fmap((val) -> (_) -> val)\appLeft(prand\filterValues(f))

_degradeBy = (by, pat) -> _degradeByWith(rand, by, pat)

degradeBy = _patternify _degradeBy

undegradeBy = _patternify _undegradeBy

_undegradeBy = (by, pat) -> _degradeByWith(rand\fmap((r) -> 1 - r), by, pat)

degrade = (pat) -> _degradeBy(0.5, pat)

undegrade = (pat) -> _undegradeBy(0.5, pat)

sometimesBy = (by, func, pat) ->
  (reify(by)\fmap (by) ->  stack _degradeBy(by, pat), func _undegradeBy(1 - by, pat))\innerJoin!

sometimes = (func, pat) -> sometimesBy(0.5, func, pat)

-- @section manipulating structure
struct = (boolpat, pat) ->
  boolpat\fmap((b) -> (val) -> if b return val else nil)\appLeft(pat)\removeNils!

_euclid = (n, k, offset, pat) -> struct bjork(n, k, offset), reify(pat)
euclid = _patternify_p_p_p _euclid

rev = (pat) ->
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

_iter = (n, pat) -> slowcat [_early Fraction(i - 1, n), pat for i in fun.range(n)]
iter = _patternify _iter

_reviter = (n, pat) -> slowcat [_late Fraction(i - 1, n), pat for i in fun.range(n)]
reviter = _patternify _reviter

_segment = (n, pat) -> fast(n, pure(id))\appLeft pat
segment = _patternify _segment

_range = (min, max, pat) -> pat\fmap((x) -> x * (max - min) + min)
range = _patternify_p_p _range

-- @section higher order functions
superimpose = (f, pat) -> stack(pat, f(pat))

layer = (table, pat) -> stack [f pat for f in *table]

_off = (time_pat, f, pat) -> stack(pat, f late(time_pat, pat))
off = _patternify_p_p _off

_echoWith = (times, time, func, pat) ->
  f = (index) -> func _late time * index, pat
  ts = [ i for i = 0, times - 1 ]
  stack map f, ts

-- TODO: test!!
echoWith = _patternify_p_p_p _echoWith

_when = (bool, f, pat) -> bool and f(pat) or pat
when_ = _patternify_p_p _when -- when is a reserved keyword ...

_firstOf = (n, f, pat) ->
  boolpat = fastcat concat { true }, [false for i = 1, n - 1]
  when_ boolpat, f, pat
firstOf = _patternify_p_p _firstOf

every = firstOf

_lastOf = (n, f, pat) ->
  boolpat = fastcat concat [ false for i = 1, n - 1 ], { true }
  when_ boolpat, f, pat
lastOf = _patternify_p_p _lastOf

_jux = (f, pat) -> _juxBy(0.5, f, pat)
jux = _patternify _jux

_juxBy = (by, f, pat) ->
  by = by / 2
  elem_or = (dict, key, default) ->
    if dict[key] != nil then return dict[key]
    return default
  left = pat\fmap((valmap) -> union { pan: elem_or(valmap, "pan", 0.5) - by }, valmap)
  right = pat\fmap((valmap) -> union { pan: elem_or(valmap, "pan", 0.5) + by }, valmap)
  return stack left, f right
juxBy = _patternify_p_p _juxBy

-- _chunk = (n, func, pat, back = false, fast = false) ->
--   pat = reify pat
--   binary = concat { true }, [ false for i = 1, n - 1 ]
--   binary_pat = _iter n, fastcat(binary) -- !back
--   when_ binary_pat, func, pat

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

slice = (npat, ipat, opat) ->
  npat\innerBind (n) ->
    ipat\outerBind (i) ->
      opat\outerBind (o) ->
        -- if type(o) != table then o = { sound: o }
        begin = if type(n) == table then begin = n[i] else begin = i / n
        _end = if type(n) == table then _end = n[i + 1] else _end = (i + 1) / n
        return pure union o, { begin: begin, end: _end, _slices: n }

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
  pat = pat .. C.speed(1 / factor) .. C.unit("c")
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

-- @section music theory
_scale = (name, pat) ->
  toScale = (v) -> getScale(name, v)
  pat\fmap(toScale)

scale = _patternify _scale

-- @section composing
-- _inhabit = (tablepat, index) ->
--   return fastcat tablepat[index]

-- helpers
apply = (x, pat) -> pat .. x
sl = string_lambda

-- TODO: wchoose, tests for the new functions
return {
  :C
  :Pattern
  :id, :pure, :silence
  :mini, :reify
  :sl, :apply
  :run, :scan
  :fastcat, :slowcat, :timecat, :randcat
  :struct, :euclid
  :stack, :layer, :superimpose
  :jux, :juxBy
  :inside, :outside
  :firstOf, :lastOf, :every
  :rev
  :off
  :when_
  :fast, :slow, :ply
  :early, :late
  :fastgap, :compress, :zoom, :focus
  :degrade, :degradeBy
  :undegradeBy, :undegrade
  :sometimes
  :iter, :reviter
  :striate, :chop
  :slice, :splice
  :loopAt
  :scale
  :sine, :sine2, :square, :square2, :saw, :saw2, :isaw, :isaw2, :tri, :tri2, :rand, :irand
}
