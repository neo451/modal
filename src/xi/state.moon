Arc = require "xi.arc"
import compare, type from require "xi.utils"

class State
  new:(arc = Arc(), controls = {}) =>
    @arc = arc
    @controls = controls

  type: -> "state"

  setArc:(arc) => State arc, @controls

  withArc:(func) => @setArc(func @arc)

  setControls:(controls) => State @arc, controls

  __eq:(other) => @arc == other.arc and compare @controls, other.controls

return State
