State = require "xi.state"
Arc = require "xi.arc"

describe "State", ->
  describe "new", ->
    it "should new with defaults", ->
      state = State!
      assert.are.same Arc(), state.arc
      assert.are.same {}, state.controls

    it "should new with args", ->
      expectedArc = Arc 1/2, 3/4
      expectedControls = { something: "something else" }
      state = State expectedArc, expectedControls
      assert.are.same expectedArc, state.arc
      assert.are.same expectedControls, state.controls

    it "should have a function declaring its type", ->
      state = State!
      assert.are.equal "state", state\type!

  describe "setArc", ->
    it "should return new state with specified arc", ->
      initialArc = Arc(1/2, 3/4)
      finalArc = Arc(2/3, 5/6)
      state = State initialArc, { something: "something else" }
      state = state\setArc finalArc
      assert.are.equal finalArc, state.arc

  describe "withArc", ->
    it "should return new state with arc modified by the function", ->
      initialArc = Arc(1/2, 3/4)
      expectedArc = Arc(2/3, 5/6)
      arcFunc = -> expectedArc 
      state = State initialArc, { something: "something else" }
      state = state\withArc(arcFunc)
      assert.are.equal(state.arc, expectedArc)

  describe "setControls", ->
    it "should return new state with specified controls", ->
      initialControls = { something: "something else" }
      finalControls = { something: "something else else" }
      state = State Arc(1/2, 3/4), initialControls
      state = state\setControls finalControls
      assert.are.equal state.controls, finalControls


  describe "equals", ->
    it "should compare all properties", ->
      state1 = State Arc(1/2, 3/4), { something: "red fish" }
      state2 = State Arc(1/2, 3/4), { something: "red fish" }
      state3 = State Arc(1/3, 3/4), { something: "red fish" }
      state4 = State Arc(1/2, 3/4), { something: "blue fish" }
      assert.is_true state1 == state2
      assert.is_false state1 == state3
      assert.is_false state1 == state4
