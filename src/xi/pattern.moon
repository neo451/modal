import map, filter, reduce, flatten, dump, type from require "xi.utils"
require "xi.fraction"
require "xi.span"
require "xi.event"
require "xi.state"

require "moon.all"

id = (a) -> a

export class Pattern
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


  -- TODO: bind
  bindWhole: (choose_whole, func) =>
    print func
    pat_val = @
    query = (_, state) ->
      withWhole = (a, b) ->
        Event (choose_whole a.whole, b.whole), b.part, b.value --context???
      match = (a) ->
        events = func(a.value)\query(state\setSpan a.part)
        map ((b) -> withWhole a, b), events
      events = pat_val\query(state)
      flatten (map ((a) -> match a), events)
    Pattern query

  outerBind:(func) =>
    @bindWhole ((a) -> a), func

  outerJoin: =>
    @outerBind id

  innerBind:(func) =>
    @bindWhole((_, b) -> b, func)

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

  -- withTime fa fb pat = withEventTime fa $ withQueryTime fb pat
  -- withTime:(q_func, e_func) =>
  --   query = @withQueryTime q_func
  --   pattern = query\withEventTime e_func

  withQueryTime:(func) =>
    @withQuerySpan (span) ->span\withTime func


  withEventTime:(func) =>
    query = (_, state) ->
      events = @query state
      time_func = (span) -> span\withTime func
      event_func = (event) -> event\withSpan time_func
      map event_func, events
    Pattern query

  onsetsOnly: =>
    @filterEvents (event) -> event\hasOnset!

  _fast:(factor) =>
    fastQuery = @withQueryTime (t) -> t * factor
    fastPattern = fastQuery\withEventTime (t) -> t / factor
    fastPattern

  _early:(offset) =>
    earlyQuery = @withQueryTime (t) -> t + offset
    earlyPattern = earlyQuery\withEventTime (t) -> t - offset
    earlyPattern

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

reify = (pat) ->
	if type(pat) == "pattern"
	  return pat
	-- if Type(pat) == "string"
	-- 	parser.mini(pat)
	-- end
	pure(pat)

export silence = -> Pattern()

export pure = (value) ->
  query = (state) =>
    cycles = state.span\spanCycles!
    m_func = (span) ->
      whole = span\wholeCycle span._begin
      Event whole, span, value
    map m_func, cycles
  Pattern query

export stack = (...) ->
  pats = { ... }
  pats = map reify, pats
  query = (state) =>
    events = map (pat) -> pat\query state, pats
    flatten events
  Pattern query

-- is prime
export slowcat = (...) ->
  pats = { ... }
  pats = map reify, pats
  query = (state) =>
    len = #pats
    index = state.span._begin\sam!\asFloat! % len + 1
    pat = pats[index]
    pat\query state
  Pattern(query)\splitQueries!

export fastcat = (...) ->
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
