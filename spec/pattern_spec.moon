require "xi.pattern"
State = require "xi.state"
Event = require "xi.event"
Arc = require "xi.arc"

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

  describe "filterEvents", ->
    it "should return new pattern with events removed based on filter func", ->
      whole1 = Arc 1/2, 2
      part1 = Arc 1/2, 1
      event1 = Event whole1, part1, 1, {}, false
      whole2 = Arc 2/3, 3
      part2 = Arc 2/3, 1
      event2 = Event whole2, part2, 2, {}, false
      events = { event1, event2 }
      pattern = Pattern -> events
      filterFunction = (e) -> e.value == 1
      filteredPattern = pattern\filterEvents filterFunction
      filteredEvents = filteredPattern\query!
      assert.are.same { event1 }, filteredEvents


  -- describe "withQueryTime", ->
  --   it "should return new pattern whose query function will pass the query timespan through a function before mapping it to events", ->
  --     pat = Pure(5)
  --     add1 = (other) -> other + Fraction 1
  --     newPat = pat\withQueryTime(add1)
  --     expectedEvents = {
  --       Event Arc(1, 2), Arc(2, 2), 5
  --     }
  --     actualEvents = newPat\queryArc(0, 1)
  --     assert.are.equal expectedEvents, actualEvents

  describe "Pure", ->
    it "should create Pattern of a single value repeating once per cycle", ->
      atom = Pure(5)
      expectedEvents = { Event Arc(0, 1), Arc(0, 1), 5, {}, false }
      actualEvents = atom\queryArc 0, 1
      assert.are.same #expectedEvents, #actualEvents
      assert.are.same expectedEvents, actualEvents
      expectedEvents = { Event Arc(0, 1), Arc(1/2, 1), 5, {}, false }
      actualEvents = atom\query State Arc 1/2, 1
      assert.are.same #expectedEvents, #actualEvents
      assert.are.same expectedEvents, actualEvents

-- 	describe("withQuerySpan", ->
-- 		it("should return new pattern with that modifies query span with function when queried", ->
-- 			 pat = Pure(5)
-- 			 newPat = pat:withQuerySpan(function(span)
-- 				return Arc(span:Time! + 0.5, span:Time! + 0.5)
-- 			)
-- 			 expectedEvents = List({
-- 				Event(Arc(0.5, 1.5), Arc(0.5, 1.5), 5),
-- 			})
-- 			assert.are.equal(expectedEvents, newPat:queryArc(Fraction(0), Fraction(1)))
-- 		)
-- 	)
--
-- 	describe("withEventTime", ->
-- 		it("should return new pattern with function mapped over event times", ->
-- 			 pat = Pure(5)
-- 			 newPat = pat:withEventTime(function(time)
-- 				return time + 0.5
-- 			)
-- 			 expectedEvents = List({
-- 				Event(Arc(0.5, 1.5), Arc(0.5, 1.5), 10),
-- 			})
-- 			assert.are.equal(expectedEvents, newPat:queryArc(0, 1))
-- 		)
-- 	)
--
-- 	describe("splitQueries", ->
-- 		it("should break a query that spans multiple cycles into multiple queries each spanning one cycle", ->
-- 			 pat = Pattern(function(state)
-- 				return List({ Event(state.span, state.span, "a") })
-- 			)
-- 			 splitPat = pat:splitQueries!
-- 			 expectedEventsPat = List({ Event(Arc(0, 2), Arc(0, 2), "a") })
-- 			 expectedEventsSplit = List({
-- 				Event(Arc(0, 1), Arc(0, 2), "a"),
-- 				Event(Arc(1, 2), Arc(0, 2), "a"),
-- 			})
-- 			assert.are.equal(expectedEventsPat, pat:queryArc(0, 2))
-- 			assert.are.equal(expectedEventsSplit, splitPat:queryArc(0, 2))
-- 		)
-- 	)
-- 	-- TODO: what is a more realistic test case than this?
-- 	--describe("outerJoin", ->
-- 	--    it("it should convert a pattern of patterns into a single pattern with time structure coming from the outer pattern"
-- 	--        , ->
-- 	--         patOfPats = Pure(Fastcat(List({ Pure("a"), Pure("b") })))
-- 	--         expectedEvents = List({
-- 	--            Event(
-- 	--                Arc(0, 1),
-- 	--                Arc(0, 1),
-- 	--                "a"
-- 	--            )
-- 	--        })
-- 	--         actualEvents = patOfPats:outerJoin!
-- 	--        assert.are.equal(expectedEvents, actualEvents:queryArc(0, 1))
-- 	--    )
-- 	--)
  describe "withValue", ->
    it "should return new pattern with function mapped over event values on query", ->
      pat = Pure(5)
      func = (v) -> v + 5
      newPat = pat\withValue func
      expectedEvents = { Event Arc(0, 1), Arc(0, 1), 10 }
      assert.are.same expectedEvents, newPat\firstCycle!


-- 	describe("onsetsOnly", ->
-- 		it("should return only events where the start of the whole equals the start of the part", ->
-- 			 whole1 = Arc(Fraction(1, 2), Fraction(2, 1))
-- 			 part1 = Arc(Fraction(1, 2), Fraction(1, 1))
-- 			 event1 = Event(whole1, part1, 1, {}, false)
-- 			 whole2 = Arc(Fraction(2, 3), Fraction(3, 1))
-- 			 part2 = Arc(Fraction(5, 6), Fraction(1, 1))
-- 			 event2 = Event(whole2, part2, 2, {}, false)
-- 			 events = List({ event1, event2 })
-- 			 p = Pattern(function(_)
-- 				return events
-- 			)
--
-- 			 patternWithOnsetsOnly = p:onsetsOnly!
--
-- 			assert.are.same(
-- 				patternWithOnsetsOnly:query(State(Arc(Fraction(0), Fraction(3)))),
-- 				List({ event1 })
-- 			)
-- 		)
--
-- 		it("pure patterns should not behave like continuous signals... they should have discrete onsets", ->
-- 			 p = Pure("bd")
--
-- 			 patternWithOnsetsOnly = p:onsetsOnly!
-- 			 expectedWhole = Arc(Fraction(0), Fraction(1))
-- 			 expectedPart = Arc(Fraction(0), Fraction(1))
-- 			 expectedEvent = Event(expectedWhole, expectedPart, "bd")
-- 			 actualEvents = patternWithOnsetsOnly:query(State(Arc(Fraction(0), Fraction(1))))
-- 			assert.are.equal(actualEvents, List({ expectedEvent }))
-- 			 querySpan = Arc(Fraction(1, 16), Fraction(1))
-- 			 state = State(querySpan)
-- 			assert.are.equal(querySpan, state:span!)
-- 			actualEvents = patternWithOnsetsOnly:query(state)
-- 			assert.are.equal(actualEvents, List({}))
-- 		)
-- 	)
-- 	describe("Slowcat", ->
-- 		it("should alternate between the patterns in the list, one pattern per cycle", ->
-- 			 cattedPats = Slowcat({ Pure(1), Pure(2), 3 })
-- 			 expectedEventsCycle1 = List({
-- 				Event(Arc(0, 1), Arc(0, 1), 1),
-- 			})
-- 			assert.are.equal(expectedEventsCycle1, cattedPats:queryArc(0, 1))
-- 			 expectedEventsCycle2 = List({
-- 				Event(Arc(1, 2), Arc(1, 2), 2),
-- 			})
-- 			assert.are.equal(expectedEventsCycle2, cattedPats:queryArc(1, 2))
--
-- 			 expectedEventsCycle3 = List({
-- 				Event(Arc(0, 1), Arc(0, 1), 3),
-- 			})
-- 			assert.are.equal(expectedEventsCycle3, cattedPats:queryArc(2, 3))
-- 			assert.are.equal(expectedEventsCycle1, cattedPats:queryArc(3, 4))
-- 		)
-- 	)
-- 	describe("fast", ->
-- 		it("should return a pattern whose events are closer together in time", ->
-- 			 pat = Pure("bd")
-- 			 expectedEvents = List({
-- 				Event(
-- 					Arc(Fraction(0), Fraction(0.5)),
-- 					Arc(Fraction(0), Fraction(0.5)),
-- 					"bd"
-- 				),
-- 				Event(
-- 					Arc(Fraction(0.5), Fraction(1)),
-- 					Arc(Fraction(0.5), Fraction(1)),
-- 					"bd"
-- 				),
-- 			})
-- 			 actualEvents = pat:fast(2):queryArc(Fraction(0), Fraction(1))
-- 			assert.are.same(expectedEvents, actualEvents)
-- 		)
-- 	)
--
-- 	describe("slow", ->
-- 		it("should return a pattern whose events are closer together in time", ->
-- 			 pat = Fastcat({ Pure("bd"), Pure("sd") })
-- 			 expectedEvents_0to1 = List({
-- 				Event(
-- 					Arc(Fraction(0), Fraction(1)),
-- 					Arc(Fraction(0), Fraction(1)),
-- 					"bd"
-- 				),
-- 			})
-- 			 expectedEvents_1to2 = List({
-- 				Event(
-- 					Arc(Fraction(1), Fraction(2)),
-- 					Arc(Fraction(1), Fraction(2)),
-- 					"sd"
-- 				),
-- 			})
-- 			 actualEvents_0to1 = pat:slow(2):queryArc(Fraction(0), Fraction(1))
-- 			 actualEvents_1to2 = pat:slow(2):queryArc(Fraction(1), Fraction(2))
-- 			assert.are.same(expectedEvents_0to1, actualEvents_0to1)
-- 			assert.are.same(expectedEvents_1to2, actualEvents_1to2)
-- 		)
-- 	)
--
-- 	describe("fastgap", ->
-- 		it("should bring pattern closer together", ->
-- 			 actualEvents = Fastcat({ Pure("bd"), Pure("sd") }):fastgap(4):firstCycle!
--
-- 			 expectedEvents = List({
-- 				Event(
-- 					Arc(Fraction(0), Fraction(1, 8)),
-- 					Arc(Fraction(0), Fraction(1, 8)),
-- 					"bd"
-- 				),
-- 				Event(
-- 					Arc(Fraction(1, 8), Fraction(1, 4)),
-- 					Arc(Fraction(1, 8), Fraction(1, 4)),
-- 					"sd"
-- 				),
-- 			})
-- 			assert.are.same(expectedEvents, actualEvents)
-- 		)
-- 	)
--
-- 	describe("compress", ->
-- 		it("should bring pattern closer together", ->
-- 			 actualEvents = Fastcat({ Pure("bd"), Pure("sd") }):compress(1 / 4, 3 / 4):firstCycle!
-- 			 expectedEvents = List({
-- 				Event(
-- 					Arc(Fraction(1, 4), Fraction(1, 2)),
-- 					Arc(Fraction(1, 4), Fraction(1, 2)),
-- 					"bd"
-- 				),
-- 				Event(
-- 					Arc(Fraction(1, 2), Fraction(3, 4)),
-- 					Arc(Fraction(1, 2), Fraction(3, 4)),
-- 					"sd"
-- 				),
-- 			})
-- 			assert.are.same(expectedEvents, actualEvents)
-- 		)
-- 	)
--
-- 	describe("timecat", ->
-- 		it("should return a pattern based one the time-pat 'tuples' passed in", ->
-- 			 actualEvents = Timecat({ { 3, Pure("bd"):fast(4) }, { 1, Pure("hh"):fast(8) } }):firstCycle!
-- 			 expectedEvents = List({
-- 				Event(
-- 					Arc(Fraction(0), Fraction(3, 16)),
-- 					Arc(Fraction(0), Fraction(3, 16)),
-- 					"bd"
-- 				),
-- 				Event(
-- 					Arc(Fraction(3, 16), Fraction(3, 8)),
-- 					Arc(Fraction(3, 16), Fraction(3, 8)),
-- 					"bd"
-- 				),
-- 				Event(
-- 					Arc(Fraction(3, 8), Fraction(9, 16)),
-- 					Arc(Fraction(3, 8), Fraction(9, 16)),
-- 					"bd"
-- 				),
-- 				Event(
-- 					Arc(Fraction(9, 16), Fraction(3, 4)),
-- 					Arc(Fraction(9, 16), Fraction(3, 4)),
-- 					"bd"
-- 				),
-- 				Event(
-- 					Arc(Fraction(3, 4), Fraction(25, 32)),
-- 					Arc(Fraction(3, 4), Fraction(25, 32)),
-- 					"hh"
-- 				),
-- 				Event(
-- 					Arc(Fraction(25, 32), Fraction(13, 16)),
-- 					Arc(Fraction(25, 32), Fraction(13, 16)),
-- 					"hh"
-- 				),
-- 				Event(
-- 					Arc(Fraction(13, 16), Fraction(27, 32)),
-- 					Arc(Fraction(13, 16), Fraction(27, 32)),
-- 					"hh"
-- 				),
-- 				Event(
-- 					Arc(Fraction(27, 32), Fraction(7, 8)),
-- 					Arc(Fraction(27, 32), Fraction(7, 8)),
-- 					"hh"
-- 				),
-- 				Event(
-- 					Arc(Fraction(7, 8), Fraction(29, 32)),
-- 					Arc(Fraction(7, 8), Fraction(29, 32)),
-- 					"hh"
-- 				),
-- 				Event(
-- 					Arc(Fraction(29, 32), Fraction(15, 16)),
-- 					Arc(Fraction(29, 32), Fraction(15, 16)),
-- 					"hh"
-- 				),
-- 				Event(
-- 					Arc(Fraction(15, 16), Fraction(31, 32)),
-- 					Arc(Fraction(15, 16), Fraction(31, 32)),
-- 					"hh"
-- 				),
-- 				Event(
-- 					Arc(Fraction(31, 32), Fraction(1)),
-- 					Arc(Fraction(31, 32), Fraction(1)),
-- 					"hh"
-- 				),
-- 			})
-- 			assert.are.same(expectedEvents, actualEvents)
-- 		)
-- 	)
-- 	-- def test_choose_cycles!:
-- 	--   assert choose_cycles("bd", "sd", "hh").query(Arc(0, 10)) == [
-- 	--       Event(Arc(0, 1), Arc(0, 1), "bd"),
-- 	--       Event(Arc(1, 2), Arc(1, 2), "sd"),
-- 	--       Event(Arc(2, 3), Arc(2, 3), "sd"),
-- 	--       Event(Arc(3, 4), Arc(3, 4), "sd"),
-- 	--       Event(Arc(4, 5), Arc(4, 5), "hh"),
-- 	--       Event(Arc(5, 6), Arc(5, 6), "bd"),
-- 	--       Event(Arc(6, 7), Arc(6, 7), "bd"),
-- 	--       Event(Arc(7, 8), Arc(7, 8), "sd"),
-- 	--       Event(Arc(8, 9), Arc(8, 9), "sd"),
-- 	--       Event(Arc(9, 10), Arc(9, 10), "bd"),
-- 	--   ]
-- 	-- def test_degrade!:
-- 	--     assert_equal_patterns(
-- 	--         pure("sd").fast(8).degrade!, pure("sd").fast(8).degrade_by(0.5, rand!)
-- 	--     )
-- 	--
-- 	--
-- 	-- def test_degrade_by!:
-- 	--     assert pure("sd").fast(8).degrade_by(0.75).first_cycle! == [
-- 	--         Event(Arc(1 / 8, 1 / 4), Arc(1 / 8, 1 / 4), "sd"),
-- 	--         Event(Arc(1 / 2, 5 / 8), Arc(1 / 2, 5 / 8), "sd"),
-- 	--         Event(Arc(3 / 4, 7 / 8), Arc(3 / 4, 7 / 8), "sd"),
-- 	--     ]
-- )
