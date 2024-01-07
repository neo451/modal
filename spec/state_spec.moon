require "xi.state"
require "xi.span"

describe "State", ->
  describe "new", ->
    it "should new with defaults", ->
      state = State!
      assert.are.same Span!, state.span
      assert.are.same {}, state.controls

    it "should new with args", ->
      expectedSpan = Span 1/2, 3/4
      expectedControls = { something: "something else" }
      state = State expectedSpan, expectedControls
      assert.are.same expectedSpan, state.span
      assert.are.same expectedControls, state.controls

    it "should have a function declaring its type", ->
      state = State!
      assert.are.equal "state", state\type!

  describe "setSpan", ->
    it "should return new state with specified span", ->
      initialSpan = Span(1/2, 3/4)
      finalSpan = Span(2/3, 5/6)
      state = State initialSpan, { something: "something else" }
      state = state\setSpan finalSpan
      assert.are.equal finalSpan, state.span

  describe "withSpan", ->
    it "should return new state with span modified by the function", ->
      initialSpan = Span(1/2, 3/4)
      expectedSpan = Span(2/3, 5/6)
      spanFunc = -> expectedSpan 
      state = State initialSpan, { something: "something else" }
      state = state\withSpan(spanFunc)
      assert.are.equal(state.span, expectedSpan)

  describe "setControls", ->
    it "should return new state with specified controls", ->
      initialControls = { something: "something else" }
      finalControls = { something: "something else else" }
      state = State Span(1/2, 3/4), initialControls
      state = state\setControls finalControls
      assert.are.equal finalControls, state.controls


  describe "equals", ->
    it "should compare all properties", ->
      state1 = State Span(1/2, 3/4), { something: "red fish" }
      state2 = State Span(1/2, 3/4), { something: "red fish" }
      state3 = State Span(1/3, 3/4), { something: "red fish" }
      state4 = State Span(1/2, 3/4), { something: "blue fish" }
      assert.is_true state1 == state2
      assert.is_false state1 == state3
      assert.is_false state1 == state4
