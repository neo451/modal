Fraction = require "xi.fraction"
Arc = require "xi.arc"
Event = require "xi.event"
State = require "xi.state"
import map, filter, reduce, flatten, type from require "xi.utils"


export class Pattern
  new:(query = -> {}) =>
    @query = query

  type: -> "pattern"

  queryArc:(b, e) =>
    arc = Arc b, e
    state = State arc
    @query state

  firstCycle: => @queryArc 0, 1

  __tostring: => @firstCycle!

  show: => @__tostring!

  filterEvents:(func) =>
    query = (state) -> filter func, @query state
    Pattern query

  onsetsOnly: =>
    @filterEvents (event) -> event\hasOnset!

  splitQueries: =>
    query = (_, state) ->
      cycles = state.arc\cycles!
      func = (arc) -> @query state\setArc arc
      flatten map func, cycles
    Pattern query

  withValue:(func) =>
    query = (_, state) ->
      events = @query state
      m_func = (event) ->
        return event\withValue func
      map m_func, events
    Pattern query

  withQueryTime:(func) =>
    query = (_, state) ->
      m_func = (arc) -> arc\withTime func
      @query state\withArc m_func
    Pattern query

export Pure = (value) ->
  query = (_, state) ->
    cycles = state.arc\cycles!
    func = (arc) ->
      whole = Arc\wholeCycle arc._begin
      Event whole, arc, value
    map func, cycles
  Pattern query
