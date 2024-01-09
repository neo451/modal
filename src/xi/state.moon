import compare from require "xi.utils"
import Span from require "xi.span"

class State
  new:(span = Span!, controls = {}) =>
    @span = span
    @controls = controls

  type: -> "state"

  setSpan:(span) => State span, @controls

  withSpan:(func) => @setSpan(func @span)

  setControls:(controls) => State @span, controls

  __eq:(other) => @span == other.span and compare @controls, other.controls

return { State: State }
