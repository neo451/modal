import Span, State, Event from require "xi.types"
import Pattern, pure, stack, slowcat, fastcat, timecat, fast, slow, early, late, inside, outside, fastgap, compress, zoom, focus, degradeBy from require "xi.pattern"

describe "Pattern", ->
  describe "new", ->
    it "should initialize with defaults", ->
      pat = Pattern!
      assert.are.same {}, pat\query State!

    it "should create with specified query", ->
      pat = Pattern( -> { Event! })
      events = pat\query State!
      assert.are.same { Event! }, events

  describe "withValue", ->
    it "should return new pattern with function mapped over event values on query", ->
      pat = pure 5
      func = (v) -> v + 5
      newPat = pat\withValue func
      expected = { Event Span(0, 1), Span(0, 1), 10 }
      assert.are.same expected, newPat 0, 1

  describe "onsetsOnly", ->
    it "should return only events where the start of the whole equals the start of the part", ->
      whole1 = Span(1/2, 2)
      part1 = Span(1/2, 1)
      event1 = Event(whole1, part1, 1, {}, false)
      whole2 = Span(2/3, 3)
      part2 = Span(5/6, 1)
      event2 = Event(whole2, part2, 2, {}, false)
      events = { event1, event2 }
      query = -> events
      p = Pattern query
      patternWithOnsetsOnly = p\onsetsOnly!
      actual = patternWithOnsetsOnly(0,3)
      assert.are.same { event1 }, actual

    it "pure patterns should not behave like continuous signals... they should have discrete onsets", ->
      p = pure "bd"
      patternWithOnsetsOnly = p\onsetsOnly!
      expected = { Event Span(0, 1), Span(0, 1), "bd" }
      actual = patternWithOnsetsOnly 0, 1
      assert.are.same expected, actual
      actual = patternWithOnsetsOnly 1/16, 1
      assert.are.same {}, actual

  describe "discreteOnly", ->
    it "should return only events where the start of the whole equals the start of the part", ->
      ev1 = { Event! }
      pat = Pattern (state) -> ev1
      pat = pat\discreteOnly!
      assert.are.same {}, pat 0, 1
      ev2 = { Event!, Event Span!, Span!, 1 }
      pat = Pattern (state) -> ev2
      pat = pat\discreteOnly!
      expected = { Event Span!, Span!, 1 }
      assert.are.same expected, pat 0, 1

  describe "filterEvents", ->
    it "should return new pattern with values removed based on filter func", ->
      pat = slowcat "bd", "sd", "hh", "mt"
      newPat = pat\filterEvents (e) -> e.value == "bd" or e.value == "hh"
      expected = {
        Event Span(0, 1), Span(0, 1), "bd"
        Event Span(2, 3), Span(2, 3), "hh"
      }
      assert.are.same expected, newPat 0, 4


  describe "pure", ->
    it "should create Pattern of a single value repeating once per cycle", ->
      atom = pure 5
      expected = { Event Span(0, 1), Span(0, 1), 5, {}, false }
      actual = atom 0, 1
      assert.are.same #expected, #actual
      assert.are.same expected, actual
      expected = { Event Span(0, 1), Span(1/2, 1), 5, {}, false }
      actual = atom 1/2, 1
      assert.are.same #expected, #actual
      assert.are.same expected, actual

  describe "withQuerySpan", ->
    it "should return new pattern with that modifies query span with function when queried", ->
      pat = pure 5
      func = (span) -> Span span._begin + 0.5, span._end + 0.5
      newPat = pat\withQuerySpan func
      expected = {
        Event Span(0, 1), Span(0.5, 1), 5
        Event Span(1, 2), Span(1, 1.5), 5
      }
      assert.are.same expected, newPat 0, 1

  describe "splitQueries", ->
    it "should break a query that spans multiple cycles into multiple queries each spanning one cycle", ->
      query = (state) => { Event state.span, state.span, "a" }
      pat = Pattern query
      splitPat = pat\splitQueries!
      expectedPat = { Event Span(0, 2), Span(0, 2), "a" }
      expectedSplit = {
        Event Span(0, 1), Span(0, 1), "a",
        Event Span(1, 2), Span(1, 2), "a",
      }
      assert.are.same expectedPat, pat 0, 2
      assert.are.same expectedSplit, splitPat 0, 2

  describe "withQueryTime", ->
    it "should return new pattern whose query function will pass the query timespan through a function before mapping it to events", ->
      pat = pure 5
      add1 = (other) -> other + 1
      newPat = pat\withQueryTime add1
      expected = { Event Span(1, 2), Span(1, 2), 5 }
      assert.are.same expected, newPat 0, 1

  describe "withEventTime", ->
    it "should return new pattern with function mapped over event times", ->
      pat = pure 5
      func = (time) -> time + 0.5
      newPat = pat\withEventTime func
      expected = { Event Span(0.5, 1.5), Span(0.5, 1.5), 5 }
      assert.are.same expected, newPat 0, 1

  describe "outerJoin", ->
    it "it should convert a pattern of patterns into a single pattern with time structure coming from the outer pattern" , ->
      patOfPats = pure fastcat "a", "b"
      pat = patOfPats\outerJoin!
      expected = {
        Event Span(0, 1), Span(0, 1/2), "a"
        Event Span(0, 1), Span(1/2, 1), "b"
      }
      assert.are.same expected, pat 0, 1

  describe "squeezeJoin", ->
    it "it should convert a pattern of patterns into a single pattern, takes whole cycles of the inner pattern to fit each event in the outer pattern.
" , ->
      patOfPats = fastcat "1 2 3", "1 2 3"
      pat = patOfPats\squeezeJoin!
      expected = {
        Event Span(0, 1/6), Span(0, 1/6), 1
        Event Span(1/6, 1/3), Span(1/6, 1/3), 2
        Event Span(1/3, 1/2), Span(1/3, 1/2), 3
        Event Span(1/2, 2/3), Span(1/2, 2/3), 1
        Event Span(2/3, 5/6), Span(2/3, 5/6), 2
        Event Span(5/6, 1), Span(5/6, 1), 3
      }
      assert.are.same expected, pat 0, 1

  describe "slowcat", ->
    it "should alternate between the patterns in the list, one pattern per cycle", ->
      pat = slowcat 1, 2, 3
      expected = {
        Event Span(0, 1), Span(0, 1), 1
        Event Span(1, 2), Span(1, 2), 2
        Event Span(2, 3), Span(2, 3), 3
      }
      assert.are.same expected, pat 0, 3

  describe "fastcat", ->
    it "should alternate between the patterns in the list, all in one cycle", ->
      pat = fastcat 1, 2, 3
      expected = {
        Event Span(0, 1/3), Span(0, 1/3), 1
        Event Span(1/3, 2/3), Span(1/3, 2/3), 2
        Event Span(2/3, 1), Span(2/3, 1), 3
      }
      assert.are.same expected, pat 0, 1

  describe "stack", ->
    it "should stack up the pats to be played together", ->
      pat = stack "bd", "sd", "hh"
      expected = {
        Event Span(0, 1), Span(0, 1), "bd"
        Event Span(0, 1), Span(0, 1), "sd"
        Event Span(0, 1), Span(0, 1), "hh"
      }
      assert.are.same expected, pat 0, 1

  describe "timecat", ->
    it "should return a pattern based one the time-pat 'tuples' passed in", ->
      pat = timecat { { 3, fast(4, "bd") }, { 1, fast(8, "hh") } }
      expected = {
        Event Span(0, 3/16), Span(0, 3/16), "bd"
        Event Span(3/16, 3/8), Span(3/16, 3/8), "bd"
        Event Span(3/8, 9/16), Span(3/8, 9/16), "bd"
        Event Span(9/16, 3/4), Span(9/16, 3/4), "bd"
        Event Span(3/4, 25/32), Span(3/4, 25/32), "hh"
        Event Span(25/32, 13/16), Span(25/32, 13/16), "hh"
        Event Span(13/16, 27/32), Span(13/16, 27/32), "hh"
        Event Span(27/32, 7/8), Span(27/32, 7/8), "hh"
        Event Span(7/8, 29/32), Span(7/8, 29/32), "hh"
        Event Span(29/32, 15/16), Span(29/32, 15/16), "hh"
        Event Span(15/16, 31/32), Span(15/16, 31/32), "hh"
        Event Span(31/32, 1), Span(31/32, 1), "hh"
      }
      assert.are.same expected, pat 0, 1

  describe "fast", ->
    it "should return a pattern whose events are closer together in time", ->
      pat = fast 2, "bd"
      expected = {
        Event Span(0, 0.5), Span(0, 0.5), "bd"
        Event Span(0.5, 1), Span(0.5, 1), "bd"
      }
      assert.are.same expected, pat 0, 1

  describe "slow", ->
    it "should return a pattern whose events are closer together in time", ->
      pat = slow 2, "bd sd"
      expected = {
        Event Span(0, 1), Span(0, 1), "bd"
        Event Span(1, 2), Span(1, 2), "sd"
      }
      assert.are.same expected, pat 0, 2

  describe "early", ->
    it "should return a pattern whose events are moved backword in time", ->
      pat = early 0.5, "bd sd"
      expected = {
        Event Span(1/2, 1), Span(1/2, 1), "bd"
        Event Span(0, 1/2), Span(0, 1/2), "sd"
      }
      assert.are.same expected, pat 0, 1

  describe "fastgap", ->
    it "should bring pattern closer together", ->
      pat = fastgap 4, "bd sd"
      expected = {
         Event Span(0, 1/8), Span(0, 1/8), "bd"
         Event Span(1/8, 1/4), Span(1/8, 1/4), "sd"
      }
      assert.are.same expected, pat 0, 1

  describe "compress", ->
    it "should bring pattern closer together", ->
      pat = compress 1/4, 3/4, "bd sd"
      expected = {
        Event Span(1/4, 1/2), Span(1/4, 1/2), "bd"
        Event Span(1/2, 3/4), Span(1/2, 3/4), "sd"
      }
      assert.are.same expected, pat 0, 1

  -- TODO: is this right?
  describe "focus", ->
    it "should bring pattern closer together, but leave no gap, and focus can be bigger than a cycle", ->
      pat = focus 1/4, 3/4, "bd sd"
      expected = {
        Event Span(1/4, 1/2), Span(1/4, 1/2), "bd"
        Event Span(3/4, 1), Span(3/4, 1), "bd"
        Event Span(0, 1/4), Span(0, 1/4), "sd"
        Event Span(1/2, 3/4), Span(1/2, 3/4), "sd"
      }
      assert.are.same expected, pat 0, 1

  describe "zoom", ->
    it "should play a portion of a pattern", ->
      pat = zoom 1/4, 3/4, "~ bd sd ~"
      expected = {
        Event Span(0, 1/2), Span(0, 1/2), "bd"
        Event Span(1/2, 1), Span(1/2, 1), "sd"
      }
      assert.are.same expected, pat 0, 1

  describe "degrade_by", ->
    it "should randomly drop events from a pattern", ->
      pat = degradeBy 0.75, fast(8, "sd")
      expected = {
          Event Span(1/8, 1/4), Span(1/8, 1/4), "sd"
          Event Span(1/2, 5/8), Span(1/2, 5/8), "sd"
          Event Span(3/4, 7/8), Span(3/4, 7/8), "sd"
      }
      assert.are.same expected, pat 0, 1
