busted = require "busted"
describe = busted.describe
it = busted.it

Fraction = require "xi.fraction"
Timespan = require "xi.timespan"

describe "Timespan", ->
  describe "wholeCycle", ->
    it "should return the large cycle that contains the span", ->
      f1 = Fraction(1, 2)
      actual = Timespan\wholeCycle f1
      expected = Timespan 0, 1
      assert.are.same expected, actual
      f2 = Fraction(3, 2)
      actual = Timespan\wholeCycle f2
      expected = Timespan 1, 2
      assert.are.same expected, actual
	describe "cyclePos", ->
		it "should return the position within the cycle as a proper fraction", ->
			f1 = Fraction(7, 2)
      actual = Timespan\cyclePos f1
      expected = Fraction 1, 2
      assert.are.same expected, actual
