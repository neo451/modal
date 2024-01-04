Fraction = require "xi.fraction"
Arc = require "xi.arc"
Event = require "xi.event"
State = require "xi.state"
import map, filter, reduce, type from require "xi.utils"


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

export Pure = (value) ->
  query = (_, state) ->
    cycles = state.arc\cycles!
    func = (subspan) ->
      whole = Arc\wholeCycle subspan._begin
      Event whole, subspan, value
    map func, cycles
  Pattern query
