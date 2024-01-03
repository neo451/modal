--TODO: intersection, intersection_e
busted = require "busted"
Fraction = require "xi.fraction"
Timespan = require "xi.timespan"

describe "Timespan", ->
  describe "wholeCycle", ->
    it "should return the large cycle that contains the span", ->
      f1 = Fraction 1, 2
      actual = Timespan\wholeCycle f1
      expected = Timespan 0, 1
      assert.are.same expected, actual
      f2 = Fraction 3, 2
      actual = Timespan\wholeCycle f2
      expected = Timespan 1, 2
      assert.are.same expected, actual

  describe "cyclePos", ->
    it "should return the position within the cycle as a proper fraction", ->
      f1 = Fraction 7, 2
      actual = Timespan\cyclePos f1
      expected = Fraction 1, 2
      assert.are.same expected, actual

  describe "new", ->
    it "should create with defaults", ->
      t = Timespan!
      assert.are.same Fraction(1), t._begin
      assert.are.same Fraction(1), t._end

    it "should create with args", ->
      t = Timespan 3, 4
      assert.are.same Fraction(3), t._begin
      assert.are.same Fraction(4), t._end

    it "should promote numbers to fractions", ->
      t = Timespan 0.5, 0.75
      assert.are.same Fraction(1, 2), t._begin
      assert.are.same Fraction(3, 4), t._end

    it "should have a type", ->
      t = Timespan!
      assert.are.same 'timespan', t\type!

  describe "spanCycles", ->
    it "should break multi cycle span into pieces", ->
      t = Timespan 3/4, 7/2
      spans = t\spanCycles!
      assert.are.same 4, #spans
      assert.are.same Fraction(3, 4), spans[1]._begin
      assert.are.same Fraction(1, 1), spans[1]._end
      assert.are.same Fraction(1, 1), spans[2]._begin
      assert.are.same Fraction(2, 1), spans[2]._end
      assert.are.same Fraction(2, 1), spans[3]._begin
      assert.are.same Fraction(3, 1), spans[3]._end
      assert.are.same Fraction(3, 1), spans[4]._begin
      assert.are.same Fraction(7, 2), spans[4]._end
    it "should preserve subcycle length spans", -> 
      t = Timespan 1/16, 1
      spans = t\spanCycles!
      assert.are.same 1, #spans
      assert.are.same Fraction(1, 16), spans[1]._begin
      assert.are.same Fraction(1, 1), spans[1]._end

  describe "duration", ->
    it "should return duration of the span", ->
      t = Timespan 3/4, 7/2
      assert.are.same Fraction(11, 4), t\duration!
      t = Timespan 6/7, 10/11
      assert.are.same Fraction(4, 77), t\duration!

  describe "midpoint", ->
    it "should return the middle point between span begin and end", ->
      t = Timespan 0, 1
      assert.are.same Fraction(1, 2), t\midpoint!
      t = Timespan 7/11, 5/4
      assert.are.same Fraction(83, 88), t\midpoint!

  describe "cycleArc", ->
    it "should return the span as if it started in cycle 0", ->
      t = Timespan 5/4, 11/4
      assert.are.same Timespan(1/4, 7/4), t\cycleArc!


  describe "equals", ->
    it "should compare properties", ->
      t1 = Timespan 1/2, 5/4
      t2 = Timespan 1/2, 5/4
      assert.are.same t1, t2
      t1 = Timespan 4/8, 5/4
      t2 = Timespan 1/2, 10/8
      assert.are.same t1, t2

  describe "withTime", ->
    it "should return new span with modified begin time", ->
      add1 = (frac) -> frac + Fraction(1)
      t = Timespan 1/2, 5/6
      actual = t\withTime add1
      expected = Timespan 3/2, 11/6
      assert.are.same expected, actual

  describe "withEnd", ->
    it "should return new span with modified end time", ->
      add1 = (frac) -> frac + Fraction(1)
      t = Timespan 1/2, 5/6
      actual = t\withEnd add1
      expected = Timespan 1/2, 11/6
      assert.are.same expected, actual

