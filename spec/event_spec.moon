import Fraction from require "xi.fraction"
import Span from require "xi.span"
import Event from require "xi.event"

describe "Event", ->
  describe "constructors", ->
    it "shoud new with default values", ->
      event = Event!
      assert.is_nil event.whole
      assert.are.same event.part, Span()
      assert.is_nil event.value
      assert.are.same event.context, {}
      assert.are.same event.stateful, false

    it 'should new with arguments', ->
      expectedWhole = Span 1/2, 1
      expectedPart = Span 1/2, 3/4
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
      whole = Span(1/2, 1)
      part = Span(1/2, 3/4)
      event = Event(whole, part, 5, {}, false)
      assert.are.equals(Fraction(1, 2), event\duration!)



    describe "wholeOrPart", ->
      it "should return whole if defined", ->
        whole = Span 1/2, 1
        part = Span 1/2, 3/4
        event = Event whole, part, 5, {}, false
        assert.are.equals whole, event\wholeOrPart!

      it "should return part if whole is not defined", ->
        part = Span 1/2, 3/4
        event = Event nil, part, 5, {}, false
        assert.are.equals part, event\wholeOrPart!

    describe "hasOnset", ->

      it "should report onset true if part and whole begin together", ->
        whole = Span 1/2, 1
        part = Span 1/2, 3/4
        event = Event whole, part, 5, {}, false
        assert.is_true event\hasOnset!

        whole = Span 1/2, 1
        part = Span 2/3, 1
        event = Event whole, part, 5, {}, false
        assert.is_false event\hasOnset!

        whole = Span 1/2, 1
        part = Span 2/3, 3/4
        event = Event whole, part, 5, {}, false
        assert.is_false event\hasOnset!

        part = Span 2/3, 3/4
        event = Event nil, part, 5, {}, false
        assert.is_false event\hasOnset!

  describe "withSpan", ->
    it "should return new event with modified span", ->
      oldPart = Span 2/3, 6/5
      oldWhole = Span 1/2, 7/5
      newPartAndWhole = Span 1/2, 3/4
      changeSpan = -> newPartAndWhole 
      event = Event oldWhole, oldPart, 5, {}, false
      newEvent = event\withSpan changeSpan
      assert.are.equals newPartAndWhole, newEvent.part
      assert.are.equals newPartAndWhole, newEvent.whole
      assert.are.equals oldPart, event.part

      event = Event nil, oldPart, 5, {}, false
      newEvent = event\withSpan changeSpan
      assert.are.equals newPartAndWhole, newEvent.part
      assert.is_nil newEvent.whole
      assert.are.equals oldPart, event.part

  describe "show", ->
    it "should produce string representation of event times", ->
      event = Event Span(1/2, 2), Span(1/2, 1), 5
      assert.are.equals(event\show!, "[(1/2 → 1/1) ⇝ | 5]")
      event = Event Span(1/2, 1), Span(1/2, 1), 6
      assert.are.equals(event\show!, "[1/2 → 1/1 | 6]")
      event = Event Span(1/2, 1), Span(3/4, 1), 6
      assert.are.equals(event\show!, "[(3/4 → 1/1) ⇜ | 6]")

  describe "withValue", ->
    it "should return new event with modified value", ->
      oldValue = 5
      add1 = (v) -> v + 1
      event = Event(nil, Span(1/2, 1), oldValue)
      newEvent = event\withValue(add1)
      assert.are.equals(newEvent.value, 6)

  describe "spanEquals", ->
    it "should report if events share a part", ->
      event1 = Event Span(1/2, 1), Span(1/2, 1), 5
      event2 = Event Span(1/2, 1), Span(3/4, 1), 5
      assert.is_true event1\spanEquals(event2)
      event3 = Event Span(0, 1), Span(1/2, 1), 5
      assert.is_false event1\spanEquals(event3)
      event4 = Event nil, Span(1/2, 1), 5
      assert.is_false event1\spanEquals(event4)
      event5 = Event nil, Span(3/4, 1), 6
      assert.is_true event4\spanEquals(event5)

  describe "equals", ->
    it "should compare all properties", ->
      event1 = Event Span(1/2, 1), Span(1/2, 1), 5, {}, false
      event2 = Event Span(1/2, 1), Span(1/2, 1), 5, {}, false
      assert.is_true event1 == event2
      event3 = Event Span(1/2, 1), Span(1/2, 1), 6, {}, false
      event4 = Event Span(1/2, 1), Span(3/4, 1), 5, {}, false
      assert.is_false(event1 == event3)
      assert.is_false(event1 == event4)
      event5 = Event Span(3/4, 1), Span(1/2, 1), 5, {}, false
      assert.is_false(event1 == event5)



  describe "combineContext", ->
    it "should return new event with merged context tables", ->
        event1 = Event Span(1/2, 1), Span(1/2, 1), 5, { thing1: "something", thing2: 5, locations: { 1, 2, 3 } }, false
        event2 = Event Span(1/2, 1), Span(1/2, 1), 6, { thing1: "something else", thing3: "more cowbell", locations: { 4, 5, 6 } }, false
        expectedContext = { thing1: "something else", thing2: 5, thing3: "more cowbell", locations: { 1, 2, 3, 4, 5, 6 } }

        assert.are.same expectedContext, event1\combineContext(event2)

        -- event1 = Event Span(1/2, 1), Span(1/2, 1), 5, { thing1: "something", thing2: 5, locations: { 1, 2, 3 } }, false
        -- event2 = Event Span(1/2, 1), Span(1/2, 1), 6, { thing1: "something else", thing3: "more cowbell" }, false
        -- expectedContext = { thing1: "something else", thing2: 5, thing3: "more cowbell", locations: { 1, 2, 3 } }
        --
        -- assert.are.same expectedContext, event1\combineContext(event2)

  describe "setContext", ->
    it "should return new event with specified context", ->
      event = Event Span(1/2, 1), Span(1/2, 1), 5, { thing: "something" }, false
      newContext = { thing2: "something else" }
      expectedEvent = Event Span(1/2, 1), Span(1/2, 1), 5, newContext, false
      actualEvent = event\setContext(newContext)
      assert.are.same expectedEvent, actualEvent




