Event = require 'xi.event'
Arc = require 'xi.arc'
Fraction = require 'xi.fraction'

describe "Event", ->
  describe "constructors", ->
    it "shoud new with default values", ->
      event = Event()
      assert.is_nil event.whole
      assert.are.same event.part, Arc()
      assert.is_nil event.value
      assert.are.same event.context, {}
      assert.are.same event.stateful, false

    it 'should new with arguments', ->
      expectedWhole = Arc 1/2, 1
      expectedPart = Arc 1/2, 3/4
      expectedContext = { field: "thing" }
      expectedValue = 5
      event = Event expectedWhole, expectedPart, expectedValue, expectedContext, false

      assert.are.equals event.whole, expectedWhole
      assert.are.Equals event.part, expectedPart
      assert.are.Equals event.value, expectedValue
      assert.are.Equals event.context, expectedContext
      assert.is_false event.stateful

      assert.has_error -> Event expectedWhole, expectedPart, expectedValue, expectedContext, true


    it "should have a function declaring its type", ->
      event = Event()
      assert.are.equal "event", event\type!

  describe "duration", ->
    it "should return duration of event in cycles", ->
      whole = Arc(1/2, 1)
      part = Arc(1/2, 3/4)
      event = Event(whole, part, 5, {}, false)
      assert.are.equals(Fraction(1, 2), event\duration!)



    -- describe("wholeOrPart", ->
    --   it("should return whole if defined", ->
    --     whole = Arc(Fraction(1, 2), Fraction(1, 1))
    --         part = Arc(Fraction(1, 2), Fraction(3, 4))
    --         event = Event(whole, part, 5, {}, false)
    --         assert.are.equals(event:wholeOrPart(), whole)
    --   )
    --     it("should return part if whole is not defined", ->
    --       part = Arc(Fraction(1, 2), Fraction(3, 4))
    --         event = Event(nil, part, 5, {}, false)
    --         assert.are.equals(event:wholeOrPart(), part)
    --     )
    -- )
    --
    -- describe("hasOnset", ->
    --
    --   it("should report onset true if part and whole begin together", ->
    --     whole = Arc(Fraction(1, 2), Fraction(1, 1))
    --         part = Arc(Fraction(1, 2), Fraction(3, 4))
    --         event = Event(whole, part, 5, {}, false)
    --         assert.is_true(event:hasOnset())
    --
    --         part = Arc(Fraction(2, 3), Fraction(1, 1))
    --         event = Event(whole, part, 5, {}, false)
    --         assert.is_false(event:hasOnset())
    --
    --         whole = Arc(Fraction(1, 2), Fraction(1, 1))
    --         part = Arc(Fraction(2, 3), Fraction(3, 4))
    --         event = Event(whole, part, 5, {}, false)
    --         assert.is_false(event:hasOnset())
    --
    --         part = Arc(Fraction(2, 3), Fraction(3, 4))
    --         event = Event(nil, part, 5, {}, false)
    --         assert.is_false(event:hasOnset())
    --   )
    --
    -- )
    --
    -- describe("withSpan", ->
    --   it("should return new event with modified span",
    --     ->
    --       oldPart = Arc(Fraction(2, 3), Fraction(6, 5))
    --             oldWhole = Arc(Fraction(1, 2), Fraction(7, 5))
    --             newPartAndWhole = Arc(Fraction(1, 2), Fraction(3, 4))
    --             changeSpan = function(_) return newPartAndWhole 
    --             event = Event(oldWhole, oldPart, 5, {}, false)
    --             newEvent = event:withSpan(changeSpan)
    --             assert.are.equals(newEvent.part, newPartAndWhole)
    --             assert.are.equals(newEvent.whole, newPartAndWhole)
    --             assert.are.equals(event.part, oldPart)
    --
    --             event = Event(nil, oldPart, 5, {}, false)
    --             newEvent = event:withSpan(changeSpan)
    --             assert.are.equals(newEvent.part, newPartAndWhole)
    --             assert.is_nil(newEvent.whole)
    --             assert.are.equals(event.part, oldPart)
    --   )
    --
    -- )
    --
    -- describe("withValue", ->
    --   it("should return new event with modified value",
    --     ->
    --       oldValue = 5
    --             add1 = function(v) return v + 1 
    --             event = Event(nil, Arc(Fraction(1, 2), Fraction(1, 1)), oldValue)
    --             newEvent = event:withValue(add1)
    --             assert.are.equals(newEvent.value, 6)
    --   )
    --
    -- )
    --
    -- describe("spanEquals", ->
    --   it("should report if events share a part",
    --     ->
    --       event1 = Event(
    --         Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 5
    --       )
    --             event2 = Event(
    --               Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 Arc(Fraction(3, 4), Fraction(1, 1)),
    --                 5
    --             )
    --             assert.is_true(event1:spanEquals(event2))
    --             event3 = Event(
    --               Arc(Fraction(0, 1), Fraction(1, 1)),
    --                 Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 5
    --             )
    --             assert.is_false(event1:spanEquals(event3))
    --             event4 = Event(
    --               nil,
    --                 Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 5
    --             )
    --             assert.is_false(event1:spanEquals(event4))
    --             event5 = Event(
    --               nil,
    --                 Arc(Fraction(3, 4), Fraction(1, 1)),
    --                 6
    --             )
    --             assert.is_true(event4:spanEquals(event5))
    --   )
    -- )
    --
    -- describe("equals", ->
    --   it("should compare all properties",
    --     ->
    --       event1 = Event(
    --         Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 5,
    --                 {},
    --                 false
    --       )
    --             event2 = Event(
    --               Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 5,
    --                 {},
    --                 false
    --             )
    --             assert.is_true(event1 == event2)
    --             event3 = Event(
    --               Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 6,
    --                 {},
    --                 false
    --             )
    --             assert.is_false(event1 == event3)
    --             event4 = Event(
    --               Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 Arc(Fraction(3, 4), Fraction(1, 1)),
    --                 5,
    --                 {},
    --                 false
    --             )
    --             assert.is_false(event1 == event4)
    --             event5 = Event(
    --               Arc(Fraction(3, 4), Fraction(1, 1)),
    --                 Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 5,
    --                 {},
    --                 false
    --             )
    --             assert.is_false(event1 == event5)
    --   )
    --
    -- )
    -- describe("combineContext", ->
    --   it("should return new event with merged context tables",
    --     ->
    --       event1 = Event(
    --         Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 5,
    --                 { thing1 = "something", thing2 = 5, locations = List({ 1, 2, 3 }) },
    --                 false
    --       )
    --
    --             event2 = Event(
    --               Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 6,
    --                 { thing1 = "something else", thing3 = "more cowbell", locations = List({ 4, 5, 6 }) },
    --                 false
    --             )
    --
    --             expectedContext = { thing1 = "something else", thing2 = 5, thing3 = "more cowbell",
    --               locations = List({ 1, 2, 3, 4, 5, 6 }) }
    --
    --             assert.are.same(event1:combineContext(event2), expectedContext)
    --
    --             event1 = Event(
    --               Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 5,
    --                 { thing1 = "something", thing2 = 5, locations = List({ 1, 2, 3 }) },
    --                 false
    --             )
    --
    --             event2 = Event(
    --               Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 6,
    --                 { thing1 = "something else", thing3 = "more cowbell" },
    --                 false
    --             )
    --
    --             expectedContext = { thing1 = "something else", thing2 = 5, thing3 = "more cowbell",
    --               locations = List({ 1, 2, 3 }) }
    --
    --             assert.are.same(event1:combineContext(event2), expectedContext)
    --   )
    --
    -- )
    --
    -- describe("setContext", ->
    --   it("should return new event with specified context",
    --     ->
    --       event = Event(
    --         Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 5,
    --                 { thing = "something" },
    --                 false
    --       )
    --             newContext = { thing2 = "something else" }
    --             expectedEvent = Event(
    --               Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 Arc(Fraction(1, 2), Fraction(1, 1)),
    --                 5,
    --                 newContext,
    --                 false
    --             )
    --             assert.are.same(event:setContext(newContext), expectedEvent)
    --   )
    --
    -- )
    --
describe "show", ->
  it "should produce string representation of event times", ->
    event = Event Arc(1/2, 2), Arc(1/2, 1), 5
    assert.are.equals(event\show!, "[(1/2 → 1/1) ⇝ | 5]")
    event = Event Arc(1/2, 1), Arc(1/2, 1), 6
    assert.are.equals(event\show!, "[1/2 → 1/1 | 6]")
    event = Event Arc(1/2, 1), Arc(3/4, 1), 6
    assert.are.equals(event\show!, "[(3/4 → 1/1) ⇜ | 6]")
