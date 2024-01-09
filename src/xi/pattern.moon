import map, filter, id, flatten, dump, type from require "xi.utils"
import Span from require "xi.span"
import Fraction from require "xi.fraction"
import Event from require "xi.event"
import State from require "xi.state"
-- TODO: maybe rename m_func
-- TODO: do patternify the strudel & tidal way
class Pattern
  new:(query = -> {}) =>
    @query = query

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
    print func
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

  innerBind:(func) => @bindWhole((_, b) -> b, func)

  outerJoin: => @outerBind id

  innerJoin: => @innerBind id

  filterEvents:(func) =>
    query = (_, state) ->
      events = @query state
      filter func, events
    Pattern query

  splitQueries: =>
    query = (_, state) ->
      cycles = state.span\spanCycles!
      m_func = (span) ->
        sub_state = state\setSpan span
        @query sub_state
      flatten map m_func, cycles
    Pattern query

  withValue:(func) =>
    query = (_, state) ->
      events = @query state
      m_func = (event) ->
        return event\withValue func
      map m_func, events
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

  _fast:(factor) => @withTime ((t) -> t * factor), ((t) -> t / factor)

  -- offset might be fraction?
  _early:(offset) => @withTime ((t) -> t + offset), ((t) -> t - offset)

  _slow:(value) => @_fast 1 / value

  _late:(offset) => @_early -offset

  -- need patternify
  fastgap:(factor) =>
    if type(factor) != "fraction"
      factor = Fraction(factor)

    if factor <= Fraction(0)
      silence!

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
      m_func = (event) ->
        event\withSpan eventSpanFunc
      map m_func, events
    Pattern(query)\splitQueries!

  compress:(b, e) =>
    if type(b) != "fraction" and type(e) != "fraction"
      b, e = Fraction(b), Fraction(e)
    if b > e or e > Fraction(1) or b > Fraction(1) or b < Fraction(0) or e < Fraction(0)
      silence!
    @fastgap(Fraction(1) / (e - b))\_late(b)

  -- _patternify:(method) =>
  --   patterned = (patSelf, ...)
  --     patArg = sequence ...
  --     return patArg\fmap((arg) -> method(patSelf, arg))\outerJoin!
  --   return patterned

silence = -> Pattern()

pure = (value) ->
  query = (state) =>
    cycles = state.span\spanCycles!
    m_func = (span) ->
      whole = span\wholeCycle span._begin
      Event whole, span, value
    map m_func, cycles
  Pattern query

reify = (pat) ->
  if type(pat) == "pattern"
    return pat
  -- if Type(pat) == "string"
  -- 	parser.mini(pat)
  -- end
  pure pat

stack = (...) ->
  pats = { ... }
  pats = map reify, pats
  query = (state) =>
    events = map (pat) -> pat\query state, pats
    flatten events
  Pattern query

-- is prime
slowcat = (...) ->
  pats = { ... }
  pats = map reify, pats
  query = (state) =>
    len = #pats
    index = state.span._begin\sam!\asFloat! % len + 1
    pat = pats[index]
    pat\query state
  Pattern(query)\splitQueries!

fastcat = (...) ->
  pats = { ... }
  pats = map reify, pats
  slowcat(...)\_fast(#pats)

--- tabke a table of key-value pairs
-- export timecat = (table) ->

-- export seq_count = (x) ->
--   if type(x) == "table"
--     print "1"
--     if #x == 1
--       print "2"
--       seq_count x[1]
--     else
--       print "3"
--       pats = map sequence, x ---???
--       fastcat(pats), #x
--   elseif type(x) == "pattern"
--     print "4"
--     x, 1
--   else
--     print "5"
--     pure(x), 1
--
-- export sequence = (x) ->
--   seq, _ = seq_count(x)
--   seq

return {
  Pattern: Pattern
  reify: reify
  silence: silence
  pure: pure
  stack: stack
  slowcat: slowcat
  fastcat: fastcat
  -- timecat: timecat
  -- seq_count: seq_count
  -- sequence: sequence
  }
