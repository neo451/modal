import Span from require "xi.span"
import Fraction from require "xi.fraction"
import Event from require "xi.event"
import State from require "xi.state"
import Pattern, pure, stack, slowcat, fastcat, timecat from require "xi.pattern"

describe "Pattern", ->
  describe "new", ->
    it "should initialize with defaults", ->
      pattern = Pattern!
      assert.are.same {}, pattern\query State!

    it "should create with specified query", ->
      pattern = Pattern( -> { Event! })
      events = pattern\query State!
      assert.are.same { Event! }, events

    it "should have a function declaring its type", ->
      pattern = Pattern!
      assert.are.equal "pattern", pattern\type!

  describe "withValue", ->
    it "should return new pattern with function mapped over event values on query", ->
      pat = pure(5)
      func = (v) -> v + 5
      newPat = pat\withValue func
      expectedEvents = { Event Span(0, 1), Span(0, 1), 10 }
      assert.are.same expectedEvents, newPat\firstCycle!

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

      actualEvents = patternWithOnsetsOnly\querySpan(0,3)
      assert.are.same { event1 }, actualEvents

    it "pure patterns should not behave like continuous signals... they should have discrete onsets", ->
      p = pure "bd"
      patternWithOnsetsOnly = p\onsetsOnly!
      expectedEvents = { Event Span(0, 1), Span(0, 1), "bd" }
      actualEvents = patternWithOnsetsOnly\firstCycle!
      assert.are.same expectedEvents, actualEvents
      actualEvents = patternWithOnsetsOnly\querySpan(1/16, 1)
      assert.are.same {}, actualEvents

  describe "filterEvents", ->
    it "should return new pattern with events removed based on filter func", ->
      whole1 = Span 1/2, 2
      part1 = Span 1/2, 1
      event1 = Event whole1, part1, 1, {}, false
      whole2 = Span 2/3, 3
      part2 = Span 2/3, 1
      event2 = Event whole2, part2, 2, {}, false
      events = { event1, event2 }
      pattern = Pattern -> events
      filterFunction = (e) -> e.value == 1
      filteredPattern = pattern\filterEvents filterFunction
      filteredEvents = filteredPattern\query!
      assert.are.same { event1 }, filteredEvents

  describe "pure", ->
    it "should create Pattern of a single value repeating once per cycle", ->
      atom = pure 5
      expectedEvents = { Event Span(0, 1), Span(0, 1), 5, {}, false }
      actualEvents = atom\querySpan 0, 1
      assert.are.same #expectedEvents, #actualEvents
      assert.are.same expectedEvents, actualEvents
      expectedEvents = { Event Span(0, 1), Span(1/2, 1), 5, {}, false }
      actualEvents = atom\query State Span 1/2, 1
      assert.are.same #expectedEvents, #actualEvents
      assert.are.same expectedEvents, actualEvents

  describe "withQuerySpan", ->
    it "should return new pattern with that modifies query span with function when queried", ->
      pat = pure 5
      func = (span) -> Span span._begin + 0.5, span._end + 0.5
      newPat = pat\withQuerySpan func
      expectedEvents = {
        Event Span(0, 1), Span(0.5, 1), 5
        Event Span(1, 2), Span(1, 1.5), 5
      }
      assert.are.same expectedEvents, newPat\querySpan 0, 1

  describe "splitQueries", ->
    it "should break a query that spans multiple cycles into multiple queries each spanning one cycle", ->
      query = (state) => { Event state.span, state.span, "a" }
      pat = Pattern query
      splitPat = pat\splitQueries!
      expectedEventsPat = { Event Span(0, 2), Span(0, 2), "a" }
      expectedEventsSplit = {
        Event Span(0, 1), Span(0, 1), "a",
        Event Span(1, 2), Span(1, 2), "a",
      }
      assert.are.same expectedEventsPat, pat\querySpan 0, 2
      assert.are.same expectedEventsSplit, splitPat\querySpan 0, 2

  describe "withQueryTime", ->
    it "should return new pattern whose query function will pass the query timespan through a function before mapping it to events", ->
      pat = pure 5
      add1 = (other) -> other + 1
      newPat = pat\withQueryTime add1
      expectedEvents = { Event Span(1, 2), Span(1, 2), 5 }
      actualEvents = newPat\firstCycle!
      assert.are.same expectedEvents, actualEvents

  describe "withEventTime", ->
    it "should return new pattern with function mapped over event times", ->
      pat = pure 5
      func = (time) -> time + 0.5
      newPat = pat\withEventTime func
      expectedEvents = { Event Span(0.5, 1.5), Span(0.5, 1.5), 5 }
      actualEvents = newPat\firstCycle!
      assert.are.same expectedEvents, actualEvents

-- TODO: what is a more realistic test case than this?
  describe "outerJoin", ->
    it "it should convert a pattern of patterns into a single pattern with time structure coming from the outer pattern" , ->
      patOfPats = pure(fastcat(pure("a"), pure("b")))
      expectedEvents = {
        Event Span(0, 1), Span(0, 1/2), "a"
        Event Span(0, 1), Span(1/2, 1), "b"
      }
      actualEvents = patOfPats\outerJoin!\firstCycle!
      assert.are.same expectedEvents, actualEvents

-- 	describe("onsetsOnly", ->
-- 		it("should return only events where the start of the whole equals the start of the part", ->
-- 			 whole1 = TimeSpan(Fraction(1, 2), Fraction(2, 1))
-- 			 part1 = TimeSpan(Fraction(1, 2), Fraction(1, 1))
-- 			 event1 = Event(whole1, part1, 1, {}, false)
-- 			 whole2 = TimeSpan(Fraction(2, 3), Fraction(3, 1))
-- 			 part2 = TimeSpan(Fraction(5, 6), Fraction(1, 1))
-- 			 event2 = Event(whole2, part2, 2, {}, false)
-- 			 events = List({ event1, event2 })
-- 			 p = Pattern(function(_)
-- 				return events
-- 			)
--
-- 			 patternWithOnsetsOnly = p:onsetsOnly!
--
-- 			assert.are.same(
-- 				patternWithOnsetsOnly:query(State(TimeSpan(Fraction(0), Fraction(3)))),
-- 				List({ event1 })
-- 			)
-- 		)
--

  describe "slowcat", ->
    it "should alternate between the patterns in the list, one pattern per cycle", ->
      cattedPats = slowcat 1, 2, 3
      expectedEventsCycle1 = { Event Span(0, 1), Span(0, 1), 1 }
      assert.are.same expectedEventsCycle1, cattedPats\querySpan(0, 1)
      expectedEventsCycle2 = { Event Span(1, 2), Span(1, 2), 2 }
      assert.are.same expectedEventsCycle2, cattedPats\querySpan(1, 2)
      expectedEventsCycle3 = { Event Span(2, 3), Span(2, 3), 3 }
      -- assert.are.same expectedEventsCycle3, cattedPats\querySpan(2, 3)
      -- assert.are.same expectedEventsCycle1, cattedPats\querySpan(3, 4)


  -- describe "fast", ->
  --   it "should return a pattern whose events are closer together in time", ->
  --     pat = pure "bd"
  --     expectedEvents = {
  --       Event(Span(0, 0.5), Span(0, 0.5), "bd")
  --       Event(Span(0.5, 1), Span(0.5, 1), "bd")
  --     }
  --     actualEvents = pat\fast(2)\firstCycle!
  --     assert.are.same(expectedEvents, actualEvents)
  --

--
-- 	describe("slow", ->
-- 		it("should return a pattern whose events are closer together in time", ->
-- 			 pat = Fastcat({ pure("bd"), pure("sd") })
-- 			 expectedEvents_0to1 = List({
-- 				Event(
-- 					Span(Fraction(0), Fraction(1)),
-- 					Span(Fraction(0), Fraction(1)),
-- 					"bd"
-- 				),
-- 			})
-- 			 expectedEvents_1to2 = List({
-- 				Event(
-- 					Span(Fraction(1), Fraction(2)),
-- 					Span(Fraction(1), Fraction(2)),
-- 					"sd"
-- 				),
-- 			})
-- 			 actualEvents_0to1 = pat:slow(2):queryspan(Fraction(0), Fraction(1))
-- 			 actualEvents_1to2 = pat:slow(2):queryspan(Fraction(1), Fraction(2))
-- 			assert.are.same(expectedEvents_0to1, actualEvents_0to1)
-- 			assert.are.same(expectedEvents_1to2, actualEvents_1to2)
-- 		)
-- 	)
--
  describe "fastgap", ->
    it "should bring pattern closer together", ->
      actualEvents = fastcat(pure("bd"), pure("sd"))\fastgap(4)\firstCycle!
      expectedEvents = {
         Event Span(0, 1/8), Span(0, 1/8), "bd",
         Event Span(1/8, 1/4), Span(1/8, 1/4), "sd",
      }
      assert.are.same expectedEvents, actualEvents

  describe "compress", ->
    it "should bring pattern closer together", ->
      actualEvents = fastcat(pure("bd"), pure("sd"))\compress(1/4, 3/4)\firstCycle!
      expectedEvents = {
        Event Span(1/4, 1/2), Span(1/4, 1/2), "bd",
        Event Span(1/2, 3/4), Span(1/2, 3/4), "sd",
      }
      assert.are.same expectedEvents, actualEvents


	describe("timecat", ->
    it("should return a pattern based one the time-pat 'tuples' passed in", ->
      actualEvents = timecat({ { 3, pure("bd")\_fast(4) }, { 1, pure("hh")\_fast(8) } })\firstCycle!
      expectedEvents = {
        Event(Span(0, (3 / 16)), Span(0, (3 / 16)), "bd"),
        Event(Span((3 / 16), 3 / 8), Span((3 / 16), 3 / 8), "bd"),
        Event(Span(3 / 8, (9 / 16)), Span(3 / 8, (9 / 16)), "bd"),
        Event(Span((9 / 16), 3 / 4), Span((9 / 16), 3 / 4), "bd"),
        Event(Span(3 / 4, (25 / 32)), Span(3 / 4, (25 / 32)), "hh"),
        Event(Span((25 / 32), (13 / 16)), Span((25 / 32), (13 / 16)), "hh"),
        Event(Span((13 / 16), (27 / 32)), Span((13 / 16), (27 / 32)), "hh"),
        Event(Span((27 / 32), 7 / 8), Span((27 / 32), 7 / 8), "hh"),
        Event(Span(7 / 8, (29 / 32)), Span(7 / 8, (29 / 32)), "hh"),
        Event(Span((29 / 32), (15 / 16)), Span((29 / 32), (15 / 16)), "hh"),
        Event(Span((15 / 16), (31 / 32)), Span((15 / 16), (31 / 32)), "hh"),
        Event(Span((31 / 32), 1), Span((31 / 32), 1), "hh"),
       }
			assert.are.same(expectedEvents, actualEvents)
		)
	)
-- 	-- def test_choose_cycles!:
-- 	--   assert choose_cycles("bd", "sd", "hh").query(Span(0, 10)) == [
-- 	--       Event(Span(0, 1), Span(0, 1), "bd"),
-- 	--       Event(Span(1, 2), Span(1, 2), "sd"),
-- 	--       Event(Span(2, 3), Span(2, 3), "sd"),
-- 	--       Event(Span(3, 4), Span(3, 4), "sd"),
-- 	--       Event(Span(4, 5), Span(4, 5), "hh"),
-- 	--       Event(Span(5, 6), Span(5, 6), "bd"),
-- 	--       Event(Span(6, 7), Span(6, 7), "bd"),
-- 	--       Event(Span(7, 8), Span(7, 8), "sd"),
-- 	--       Event(Span(8, 9), Span(8, 9), "sd"),
-- 	--       Event(Span(9, 10), Span(9, 10), "bd"),
-- 	--   ]
-- 	-- def test_degrade!:
-- 	--     assert_equal_patterns(
-- 	--         pure("sd").fast(8).degrade!, pure("sd").fast(8).degrade_by(0.5, rand!)
-- 	--     )
-- 	--
-- 	--
-- 	-- def test_degrade_by!:
-- 	--     assert pure("sd").fast(8).degrade_by(0.75).first_cycle! == [
-- 	--         Event(Span(1 / 8, 1 / 4), Span(1 / 8, 1 / 4), "sd"),
-- 	--         Event(Span(1 / 2, 5 / 8), Span(1 / 2, 5 / 8), "sd"),
-- 	--         Event(Span(3 / 4, 7 / 8), Span(3 / 4, 7 / 8), "sd"),
-- 	--     ]
-- )
