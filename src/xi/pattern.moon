import map, filter, reduce, flatten, type from require "xi.utils"
require "xi.fraction"
require "xi.span"
require "xi.event"
require "xi.state"

require "moon.all"

export class Pattern
  new:(query = -> {}) =>
    @query = query

  type: -> "pattern"

  querySpan:(b, e) =>
    span = Span b, e
    state = State span
    @query state

  firstCycle: => @querySpan 0, 1

  __tostring: => @firstCycle!

  show: => @__tostring!

  filterEvents:(func) =>
    query = (state) -> filter func, @query state
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
    query = (_, state) ->
      m_func = (span) -> span\withTime func
      @query state\withSpan m_func
    Pattern query

  withEventTime:(func) =>
    query = (_, state) ->
      events = @query state
      time_func = (span) -> span\withTime func
      event_func = (event) -> event\withSpan time_func
      map event_func, events
    Pattern query

  _fast:(factor) =>
    fastQuery = @withQueryTime (t) -> t * factor
    fastPattern = fastQuery\withEventTime (t) -> t / factor
    fastPattern

  onsetsOnly: =>
    @filterEvents (event) -> event\hasOnset!

  -- _patternify:(method) =>

reify = (pat) ->
	if type(pat) == "pattern"
	  return pat
	-- if Type(pat) == "string"
	-- 	parser.mini(pat)
	-- end
	return pure(pat)

export pure = (value) ->
  query = (state) =>
    cycles = state.span\spanCycles!
    m_func = (span) ->
      whole = span\wholeCycle span._begin
      print span, whole
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
