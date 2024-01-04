Fraction = require "xi.fraction"
Arc = require "xi.arc"
Event = require "xi.event"
State = require "xi.state"

class Pattern
  new:(query = -> {}) =>
    @query = query

  type: -> "pattern"

  queryArc:(b, e) =>
    arc = Arc b, e
    state = State arc
    @query(state)

  filterEvents:(func) =>
    query = (state) -> filter func @query, state
    Pattern query

  onsetsOnly: => @filterEvents (event) -> event\hasOnset!

  withQueryArc:(func) =>
    query = (state) -> @query state\withArc func
    Pattern query

  -- TODO: withQueryTime, withEventTime
  withValue:(func) => Pattern (state) -> map @query state ((e, _) -> e:withValue(func))

  fmap:(func) => @withValue(func)

  splitQueries: =>
    (state) =>
      cycles = state.span\spanCycles!
      flatten map cyles (subspan) => @query state\setSpan subspan
