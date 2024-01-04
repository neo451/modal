--TODO: intersection, intersection_e
busted = require "busted"
Fraction = require "xi.fraction"
Arc = require "xi.arc"

describe "Arc", ->
  describe "wholeCycle", ->
    it "should return the large cycle that contains the span", ->
      f1 = Fraction 1, 2
      actual = Arc\wholeCycle f1
      expected = Arc 0, 1
      assert.are.same expected, actual
      f2 = Fraction 3, 2
      actual = Arc\wholeCycle f2
      expected = Arc 1, 2
      assert.are.same expected, actual

  describe "cyclePos", ->
    it "should return the position within the cycle as a proper fraction", ->
      f1 = Fraction 7, 2
      actual = Arc\cyclePos f1
      expected = Fraction 1, 2
      assert.are.same expected, actual

  describe "new", ->
    it "should create with defaults", ->
      t = Arc!
      assert.are.same Fraction(1), t._begin
      assert.are.same Fraction(1), t._end

    it "should create with args", ->
      t = Arc 3, 4
      assert.are.same Fraction(3), t._begin
      assert.are.same Fraction(4), t._end

    it "should promote numbers to fractions", ->
      t = Arc 0.5, 0.75
      assert.are.same Fraction(1, 2), t._begin
      assert.are.same Fraction(3, 4), t._end

    it "should have a type", ->
      t = Arc!
      assert.are.same 'arc', t\type!

  describe "cycles", ->
    it "should break multi cycle span into pieces", ->
      t = Arc 3/4, 7/2
      spans = t\cycles!
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
      t = Arc 1/16, 1
      arcs = t\cycles!
      assert.are.same 1, #arcs
      assert.are.same Fraction(1, 16), arcs[1]._begin
      assert.are.same Fraction(1, 1), arcs[1]._end

  describe "duration", ->
    it "should return duration of the span", ->
      t = Arc 3/4, 7/2
      assert.are.same Fraction(11, 4), t\duration!
      t = Arc 6/7, 10/11
      assert.are.same Fraction(4, 77), t\duration!

  describe "midpoint", ->
    it "should return the middle point between span begin and end", ->
      t = Arc 0, 1
      assert.are.same Fraction(1, 2), t\midpoint!
      t = Arc 7/11, 5/4
      assert.are.same Fraction(83, 88), t\midpoint!

  describe "cycleArc", ->
    it "should return the span as if it started in cycle 0", ->
      t = Arc 5/4, 11/4
      assert.are.same Arc(1/4, 7/4), t\cycleArc!


  describe "equals", ->
    it "should compare properties", ->
      t1 = Arc 1/2, 5/4
      t2 = Arc 1/2, 5/4
      assert.are.same t1, t2
      t1 = Arc 4/8, 5/4
      t2 = Arc 1/2, 10/8
      assert.are.same t1, t2

  describe "withTime", ->
    it "should return new span with modified begin time", ->
      add1 = (frac) -> frac + Fraction(1)
      t = Arc 1/2, 5/6
      actual = t\withTime add1
      expected = Arc 3/2, 11/6
      assert.are.same expected, actual

  describe "withEnd", ->
    it "should return new span with modified end time", ->
      add1 = (frac) -> frac + Fraction(1)
      t = Arc 1/2, 5/6
      actual = t\withEnd add1
      expected = Arc 1/2, 11/6
      assert.are.same expected, actual

  describe "intersection", ->
    it "should return the common timespan between two spans", ->
      ts1 = Arc 1/2, 5/4
      ts2 = Arc 2/3, 2/2
      expected = Arc 2/3, 2/2
      assert.are.same Arc(2/3, 2/2), ts1\intersection(ts2)
      assert.are.same Arc(2/3, 2/2), ts2\intersection(ts1)
      ts1 = Arc 1/2, 5/4
      ts2 = Arc 5/4, 7/4
      assert.is_nil ts1\intersection ts2
      assert.is_nil ts2\intersection ts1

  describe "intersection_e", ->
    it "should return the common timespan between two spans", ->
      ts1 = Arc 1/2, 5/4
      ts2 = Arc 2/3, 2/2
      expected = Arc 2/3, 2/2
      assert.are.same Arc(2/3, 2/2), ts1\intersection(ts2)
      assert.are.same Arc(2/3, 2/2), ts2\intersection(ts1)
      ts1 = Arc 1/2, 5/4
      ts2 = Arc 5/4, 7/4
      assert.has_error ( ()-> ts1\intersection ts2)
      assert.has_error ( ()-> ts2\intersection ts1)
