import map, filter, id, flatten, totable, dump, timeToRand, type from require "xi.utils"
import Span from require "xi.span"
import Fraction, tofrac, tofloat from require "xi.fraction"
import Event from require "xi.event"
import State from require "xi.state"
import Parse from require "xi.mini.grammar"
import Visitor from require "xi.mini.visitor"

P = {}

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
      deg_ratio = es[1][3] or 0 --??
      pats = [e[2] for e in *es]
      table.insert tc_args, { #es * weight, P.fastcat(pats)\degrade_by(deg_ratio) }
    return P.timecat tc_args

  element:(node) =>
    modifiers = [ @eval(mod) for mod in *node.modifiers ]
    pat = @eval(node.value)
    values = { { 1, pat, 0 } }

    for modifier in *modifiers
      -- values = flatten [ modifier(v) for v in *values ]
      -- HACK: 
      values = modifier(values[1])

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
        return (w_p) -> { { node.value, w_p[2], w_p[3] }}
      when "degrade"
        arg = node.value
        switch arg.op
          when "count"
            return (w_p) -> { { w_p[1], w_p[2], Fraction(arg.value, arg.value + 1) } }
          when "value"
            return (w_p) -> { { w_p[1], w_p[2], arg.value } }
    return id

  number:(node) => P.pure node.value

  -- TODO: if index
  word:(node) =>
    P.pure node.value

  rest:(node) => silence!

parse = (code) ->
  raw_ast = Parse code
  return Visitor\visit raw_ast

P.mini = (source) ->
  ast = parse source
  return Interpreter\eval ast

class Pattern
  new:(query = -> {}) =>
    @query = query

  type: -> "pattern"

  querySpan:(b, e) =>
    span = Span b, e
    state = State span
    @query state

  firstCycle: => @querySpan 0, 1
  -- TODO: can not print signals properly
  __tostring: =>
    func = (event) -> event\show!
    dump map func, @firstCycle!

  show: => @__tostring!

  __eq: (other) => @show! == other\show! --????

  bindWhole: (choose_whole, func) =>
    pat_val = @
    query = (_, state) ->
      withWhole = (a, b) ->
        Event (choose_whole a.whole, b.whole), b.part, b.value --context?
      match = (a) ->
        events = func(a.value)\query(state\setSpan a.part)
        map ((b) -> withWhole a, b), events
      events = pat_val\query(state)
      flatten (map ((a) -> match a), events)
    Pattern query

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

  -- metamethods
  __mul:(other) => @fmap((x) -> (y) -> y * x)\appLeft(P.reify(other))

  __add:(other) => @fmap((x) -> (y) -> y + x)\appLeft(P.reify(other))

  _patternify:(method) =>
    patterned = (...) ->
      -- print "...", ...
      patArg = P.fastcat ...
      -- print patArg
      return patArg\fmap((arg) -> method(arg))\innerJoin!
    return patterned

  _fast:(factor) => @withTime ((t) -> t * factor), ((t) -> t / factor)

  fast:(...) => Pattern\_patternify((val) -> @_fast(val))(...)
  slow:(...) => Pattern\_patternify((val) -> @_slow(val))(...)

  -- offset might be fraction?
  _early:(offset) => @withTime ((t) -> t + offset), ((t) -> t - offset)

  _slow:(value) => @_fast 1 / value

  _late:(offset) => @_early -offset
  
  -- TODO: test
  segment:(n) => P.pure(id)\fast(n)\appLeft(@)

  range:(min, max) => @ * (max - min) + min

  -- need patternify
  fastgap:(factor) =>
    factor = tofrac(factor)

    if factor <= Fraction(0)
      P.silence!

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
      events = @query new_state
      f = (event) ->
        event\withSpan eventSpanFunc
      map f, events
    Pattern(query)\splitQueries!

  compress:(b, e) =>
    b, e = tofrac(b), tofrac(e)
    if b > e or e > Fraction(1) or b > Fraction(1) or b < Fraction(0) or e < Fraction(0)
      return P.silence!
    @fastgap(Fraction(1) / (e - b))\_late(b)

  degrade_by:(by, prand = nil) =>
    if by == 0
      return @
    if not prand
      prand = P.rand!
    f = (v) ->
      v > tofloat(by)
    return @fmap((val) -> (_) -> val)\appLeft(prand\filterValues(f))

  degrade: => @degrade_by(0.5)

  -- TODO: test
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
            table.insert events, Event new_whole, new_part, new_value
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
            table.insert events, Event new_whole, new_part, new_value
        return events
    Pattern query

P.silence = -> Pattern!

P.pure = (value) ->
  query = (state) =>
    cycles = state.span\spanCycles!
    f = (span) ->
      whole = span\wholeCycle span._begin
      Event whole, span, value
    map f, cycles
  Pattern query

P.reify = (pat) ->
  if type(pat) == "pattern"
    return pat
  if type(pat) == "string"
  	return P.mini pat
  P.pure pat

P.stack = (...) ->
  pats = totable(...)
  pats = map P.reify, pats
  query = (state) =>
    events = map ((pat) -> pat\query state), pats
    flatten events
  Pattern query

-- is prime
P.slowcat = (...) ->
  pats = totable(...)
  pats = map P.reify, pats
  query = (state) =>
    len = #pats
    index = state.span._begin\sam!\asFloat! % len + 1
    pat = pats[index]
    pat\query state
  Pattern(query)\splitQueries!

P.fastcat = (...) ->
  pats = totable(...)
  pats = map P.reify, pats
  P.slowcat(...)\_fast(#pats)

P.timecat = (time_pat_tuples) ->
  tuples, arranged = {}, {}
  accum, total = 0, 0
  for tup in *time_pat_tuples
    { time, pat } = tup
    table.insert tuples, { time, pat }
    total = total + time

  for tup in *tuples
    { time, pat } = tup
    table.insert arranged, { accum / total, (accum + time) / total, P.reify(pat) }
    accum = accum + time

  n_pats = {}
  for tab in *arranged
    { b, e, pat } = tab
    table.insert n_pats, pat\compress(b, e)

  P.stack(n_pats)

signal = (func) ->
  query = (state) =>
    return { Event nil, state.span, func state.span\midpoint! }
  Pattern query

-- TODO: move to utils when finished


P.rand = -> signal timeToRand -- HACK: is this right?

_chooseWith = (pat, ...) ->
  vals = totable(...)
  vals = map P.reify, vals
  if #vals == 0
    return P.silence!
  
  return pat\range(1, #vals + 1)\fmap((i) ->
    key = math.min(math.max(math.floor(i), 0), #vals) -- ????
    return vals[key]
  )

chooseWith = (pat, ...) ->
  _chooseWith(pat, ...)\outerJoin!

choose = (...) -> chooseWith P.rand!, ...

P.chooseCycles = (...) -> return choose(...)\segment(1)

P.randcat = (...) -> P.chooseCycles(...)

-- seq_count = (x) ->
--   if type(x) == "table"
--     if #x == 0
--       return { P.silence!, 0 }
--     elseif #x == 1
--       return seq_count x[1]
--     else
--       pats = map ((a) -> _seq_count(a)[1]), x ---???
--       return { P.fastcat(pats), #x }
--   elseif type(x) == "pattern"
--     return { x, 1 }
--   else
--     return { P.reify(x), 1 }
--
-- P.polymeter = (steps, ...) ->
--   args = totable(...)
--   seqs = map seq_count, args
--   p seqs
--   if #seqs == 0
--     return P.silence!
--
--   if steps == 0
--     steps = seqs[1][2]
--
--   pats = {}
--
--   for seq in *seqs
--     print Fraction(steps, seq[2])
--     if seq[2] == 0
--       continue
--     if seq[2] == steps
--       table.insert pats, seq[1]
--     else
--       print seq[1]\_fast(4)
--       table.insert pats, seq[1]\_fast(Fraction(steps, seq[2]))
--
--   return P.stack(pats)
--
-- print P.polymeter(4, "bd", "sd", "hh", "cp", "hh")

P.polyrhythm = (...) -> P.stack(...)
P.fast = (arg, pat) -> P.reify(pat)\fast(arg)
P.slow = (arg, pat) -> P.reify(pat)\slow(arg)
P.degrade = (arg, pat) -> P.reify(pat)\degrade(arg)

P.Pattern = Pattern

return P
