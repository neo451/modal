local it = require("busted").it
local assert = require("busted").assert
local describe = require("busted").describe
local types = require "modal.types"
local Span, State = types.Span, types.State

describe("new", function()
   it("should new with defaults", function()
      local state = State()
      assert.are.same(Span(), state.span)
      assert.are.same({}, state.controls)
   end)
   it("should new with args", function()
      local expectedSpan = Span(1 / 2, 3 / 4)
      local expectedControls = { something = "something else" }
      local state = State(expectedSpan, expectedControls)
      assert.are.same(expectedSpan, state.span)
      assert.are.same(expectedControls, state.controls)
   end)
end)

describe("setSpan", function()
   it("should return new state with specified span", function()
      local initialSpan = Span(1 / 2, 3 / 4)
      local finalSpan = Span(2 / 3, 5 / 6)
      local state = State(initialSpan, { something = "something else" })
      state = state:setSpan(finalSpan)
      assert.are.equal(finalSpan, state.span)
   end)
end)

describe("withSpan", function()
   it("should return new state with span modified by the function", function()
      local initialSpan = Span(1 / 2, 3 / 4)
      local expectedSpan = Span(2 / 3, 5 / 6)
      local spanFunc
      spanFunc = function()
         return expectedSpan
      end
      local state = State(initialSpan, { something = "something else" })
      state = state:withSpan(spanFunc)
      assert.are.equal(state.span, expectedSpan)
   end)
end)

describe("setControls", function()
   it("should return new state with specified controls", function()
      local initialControls = { something = "something else" }
      local finalControls = { something = "something else else" }
      local state = State(Span(1 / 2, 3 / 4), initialControls)
      state = state:setControls(finalControls)
      assert.are.equal(finalControls, state.controls)
   end)
end)

describe("equals", function()
   it("should compare all properties", function()
      local state1 = State(Span(1 / 2, 3 / 4), { something = "red fish" })
      local state2 = State(Span(1 / 2, 3 / 4), { something = "red fish" })
      local state3 = State(Span(1 / 3, 3 / 4), { something = "red fish" })
      local state4 = State(Span(1 / 2, 3 / 4), { something = "blue fish" })
      assert.is_true(state1 == state2)
      assert.is_false(state1 == state3)
      assert.is_false(state1 == state4)
   end)
end)
