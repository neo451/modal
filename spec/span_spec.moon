import Span from require "modal.types"
import Fraction from require "modal.fraction"

describe "Span", ->
  describe "wholeCycle", ->
    it "should return the large cycle that contains the span", ->
      f1 = Fraction 1, 2
      actual = Span\wholeCycle f1
      expected = Span 0, 1
      assert.are.same expected, actual
      f2 = Fraction 3, 2
      actual = Span\wholeCycle f2
      expected = Span 1, 2
      assert.are.same expected, actual

  describe "cyclePos", ->
    it "should return the position within the cycle as a proper fraction", ->
      f1 = Fraction 7, 2
      actual = Span\cyclePos f1
      expected = Fraction 1, 2
      assert.are.same expected, actual

  describe "new", ->
    it "should create with defaults", ->
      t = Span!
      assert.are.same Fraction(1), t._begin
      assert.are.same Fraction(1), t._end

    it "should create with args", ->
      t = Span 3, 4
      assert.are.same Fraction(3), t._begin
      assert.are.same Fraction(4), t._end

    it "should promote numbers to fractions", ->
      t = Span 0.5, 0.75
      assert.are.same Fraction(1, 2), t._begin
      assert.are.same Fraction(3, 4), t._end

  describe "spanCycles", ->
    it "should break multi cycle span into pieces", ->
      t = Span 3/4, 7/2
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
      t = Span 1/16, 1
      spans = t\spanCycles!
      assert.are.same 1, #spans
      assert.are.same Fraction(1, 16), spans[1]._begin
      assert.are.same Fraction(1, 1), spans[1]._end

  describe "duration", ->
    it "should return duration of the span", ->
      t = Span 3/4, 7/2
      assert.are.same Fraction(11, 4), t\duration!
      t = Span 6/7, 10/11
      assert.are.same Fraction(4, 77), t\duration!

  describe "midpoint", ->
    it "should return the middle point between span begin and end", ->
      t = Span 0, 1
      assert.are.same Fraction(1, 2), t\midpoint!
      t = Span 7/11, 5/4
      assert.are.same Fraction(83, 88), t\midpoint!

  describe "cycleSpan", ->
    it "should return the span as if it started in cycle 0", ->
      t = Span 5/4, 11/4
      assert.are.same Span(1/4, 7/4), t\cycleSpan!

  describe "equals", ->
    it "should compare properties", ->
      t1 = Span 1/2, 5/4
      t2 = Span 1/2, 5/4
      assert.are.same t1, t2
      t1 = Span 4/8, 5/4
      t2 = Span 1/2, 10/8
      assert.are.same t1, t2

  describe "withTime", ->
    it "should return new span with modified begin time", ->
      add1 = (frac) -> frac + Fraction(1)
      t = Span 1/2, 5/6
      actual = t\withTime add1
      expected = Span 3/2, 11/6
      assert.are.same expected, actual

  describe "withEnd", ->
    it "should return new span with modified end time", ->
      add1 = (frac) -> frac + Fraction(1)
      t = Span 1/2, 5/6
      actual = t\withEnd add1
      expected = Span 1/2, 11/6
      assert.are.same expected, actual

  describe "intersection", ->
    it "should return the common timespan between two spans", ->
      ts1 = Span 1/2, 5/4
      ts2 = Span 2/3, 2/2
      expected = Span 2/3, 2/2
      assert.are.same Span(2/3, 2/2), ts1\sect(ts2)
      assert.are.same Span(2/3, 2/2), ts2\sect(ts1)
      ts1 = Span 1/2, 5/4
      ts2 = Span 5/4, 7/4
      assert.is_nil ts1\sect ts2
      assert.is_nil ts2\sect ts1

  describe "intersection_e", ->
    it "should return the common timespan between two spans", ->
      ts1 = Span 1/2, 5/4
      ts2 = Span 2/3, 2/2
      expected = Span 2/3, 2/2
      assert.are.same Span(2/3, 2/2), ts1\sect_e(ts2)
      assert.are.same Span(2/3, 2/2), ts2\sect_e(ts1)
      ts1 = Span 1/2, 5/4
      ts2 = Span 5/4, 7/4
      assert.has_error ( ()-> ts1\sect_e ts2)
      assert.has_error ( ()-> ts2\sect_e ts1)
