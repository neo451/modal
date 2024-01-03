class Pattern
  new:(query = {}) =>
    @query = query

  type: -> "pattern"

  queryArc:(b, e) =>
    span = Timespan(b, e)
    state = State(span)
    @query(state)

  filterEvents:(func) => (state) -> @query(state)\filter(func)

  onsetsOnly: => @filterEvents (event) -> event\hasOnset!

  withQuerySpan:(func) => Pattern (state) -> @query state\withSpan func

  -- TODO: withQueryTime, withEventTime
  withValue:(func) => Pattern (state) -> map @query state ((e, _) -> e:withValue(func))

  fmap:(func) => @withValue(func)

  splitQueries: =>
    (state) => 
      cycles = state.span\spanCycles!
      flatten map cyles (subspan) => @query state\setSpan subspan
