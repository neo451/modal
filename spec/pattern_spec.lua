local describe = require("busted").describe
local it = require("busted").it
local assert = require("busted").assert

local Span, State, Event
do
   local _obj_0 = require("modal.types")
   Span, State, Event = _obj_0.Span, _obj_0.State, _obj_0.Event
end
local Pattern, reify, pure, stack, slowcat, fastcat, timecat, fast, slow, early, late, inside, outside, fastgap, compress, zoom, focus, degradeBy, striate, chop, slice, splice, iter
do
   local _obj_0 = require("modal.pattern")
   Pattern, reify, pure, stack, slowcat, fastcat, timecat, fast, slow, early, late, inside, outside, fastgap, compress, zoom, focus, degradeBy, striate, chop, slice, splice, iter =
      _obj_0.Pattern,
      _obj_0.reify,
      _obj_0.pure,
      _obj_0.stack,
      _obj_0.slowcat,
      _obj_0.fastcat,
      _obj_0.timecat,
      _obj_0.fast,
      _obj_0.slow,
      _obj_0.early,
      _obj_0.late,
      _obj_0.inside,
      _obj_0.outside,
      _obj_0.fastgap,
      _obj_0.compress,
      _obj_0.zoom,
      _obj_0.focus,
      _obj_0.degradeBy,
      _obj_0.striate,
      _obj_0.chop,
      _obj_0.slice,
      _obj_0.splice,
      _obj_0.iter
end
do
   local _obj_0 = require("modal.ui")
   striate, chop, slice, splice = _obj_0.striate, _obj_0.chop, _obj_0.slice, _obj_0.splice
end
local C = require("modal.params")

-- local reify = require("modal.pattern").modal

describe("new", function()
   it("should initialize with defaults", function()
      local pat = Pattern()
      assert.are.same({}, pat:query(State()))
   end)
   it("should create with specified query", function()
      local pat = Pattern(function()
         return {
            Event(),
         }
      end)
      local events = pat:query(State())
      assert.are.same({
         Event(),
      }, events)
   end)
end)

describe("withValue", function()
   it("should return new pattern with function mapped over event values on query", function()
      local pat = pure(5)
      local func
      func = function(v)
         return v + 5
      end
      local newPat = pat:withValue(func)
      local expected = {
         Event(Span(0, 1), Span(0, 1), 10),
      }
      return assert.are.same(expected, newPat(0, 1))
   end)
end)

describe("onsetsOnly", function()
   it("should return only events where the start of the whole equals the start of the part", function()
      local whole1 = Span(1 / 2, 2)
      local part1 = Span(1 / 2, 1)
      local event1 = Event(whole1, part1, 1, {}, false)
      local whole2 = Span(2 / 3, 3)
      local part2 = Span(5 / 6, 1)
      local event2 = Event(whole2, part2, 2, {}, false)
      local events = {
         event1,
         event2,
      }
      local query
      query = function()
         return events
      end
      local p = Pattern(query)
      local patternWithOnsetsOnly = p:onsetsOnly()
      local actual = patternWithOnsetsOnly(0, 3)
      return assert.are.same({
         event1,
      }, actual)
   end)
   return it("pure patterns should not behave like continuous signals... they should have discrete onsets", function()
      local p = pure("bd")
      local patternWithOnsetsOnly = p:onsetsOnly()
      local expected = {
         Event(Span(0, 1), Span(0, 1), "bd"),
      }
      local actual = patternWithOnsetsOnly(0, 1)
      assert.are.same(expected, actual)
      actual = patternWithOnsetsOnly(1 / 16, 1)
      return assert.are.same({}, actual)
   end)
end)

describe("discreteOnly", function()
   return it("should return only events where the start of the whole equals the start of the part", function()
      local ev1 = { Event() }
      local pat = Pattern(function(state)
         return ev1
      end)
      pat = pat:discreteOnly()
      assert.are.same({}, pat(0, 1))
      local ev2 = {
         Event(),
         Event(Span(), Span(), 1),
      }
      pat = Pattern(function(state)
         return ev2
      end)
      pat = pat:discreteOnly()
      local expected = {
         Event(Span(), Span(), 1),
      }
      return assert.are.same(expected, pat(0, 1))
   end)
end)
describe("filterEvents", function()
   return it("should return new pattern with values removed based on filter func", function()
      local pat = slowcat(reify("bd"), reify("sd"), reify("hh"), reify("mt"))
      local newPat = pat:filterEvents(function(e)
         return e.value == "bd" or e.value == "hh"
      end)
      local expected = {
         Event(Span(0, 1), Span(0, 1), "bd"),
         Event(Span(2, 3), Span(2, 3), "hh"),
      }
      return assert.are.same(expected, newPat(0, 4))
   end)
end)
describe("withQuerySpan", function()
   it("should return new pattern with that modifies query span with function when queried", function()
      local pat = pure(5)
      local func = function(span)
         return Span(span._begin + 0.5, span._end + 0.5)
      end
      local newPat = pat:withQuerySpan(func)
      local expected = {
         Event(Span(0, 1), Span(0.5, 1), 5),
         Event(Span(1, 2), Span(1, 1.5), 5),
      }
      assert.are.same(expected, newPat(0, 1))
   end)
end)
describe("splitQueries", function()
   it("should break a query that spans multiple cycles into multiple queries each spanning one cycle", function()
      local query
      query = function(self, state)
         return {
            Event(state.span, state.span, "a"),
         }
      end
      local pat = Pattern(query)
      local splitPat = pat:splitQueries()
      local expectedPat = {
         Event(Span(0, 2), Span(0, 2), "a"),
      }
      local expectedSplit = {
         Event(Span(0, 1), Span(0, 1), "a"),
         Event(Span(1, 2), Span(1, 2), "a"),
      }
      assert.are.same(expectedPat, pat(0, 2))
      assert.are.same(expectedSplit, splitPat(0, 2))
   end)
end)
describe("withQueryTime", function()
   it(
      "should return new pattern whose query function will pass the query timespan through a function before mapping it to events",
      function()
         local pat = pure(5)
         local add1
         add1 = function(other)
            return other + 1
         end
         local newPat = pat:withQueryTime(add1)
         local expected = {
            Event(Span(1, 2), Span(1, 2), 5),
         }
         assert.are.same(expected, newPat(0, 1))
      end
   )
end)
describe("withEventTime", function()
   it("should return new pattern with function mapped over event times", function()
      local pat = pure(5)
      local func
      func = function(time)
         return time + 0.5
      end
      local newPat = pat:withEventTime(func)
      local expected = {
         Event(Span(0.5, 1.5), Span(0.5, 1.5), 5),
      }
      assert.are.same(expected, newPat(0, 1))
   end)
end)
describe("outerJoin", function()
   it(
      "it should convert a pattern of patterns into a single pattern with time structure coming from the outer pattern",
      function()
         local patOfPats = pure(fastcat(reify("a"), reify("b")))
         local pat = patOfPats:outerJoin()
         local expected = {
            Event(Span(0, 1), Span(0, 1 / 2), "a"),
            Event(Span(0, 1), Span(1 / 2, 1), "b"),
         }
         assert.are.same(expected, pat(0, 1))
      end
   )
end)
describe("squeezeJoin", function()
   it(
      "it should convert a pattern of patterns into a single pattern, takes whole cycles of the inner pattern to fit each event in the outer pattern.\n ",
      function()
         local patOfPats = fastcat(fastcat(reify("1 2 3")), fastcat(reify("1 2 3")))
         local pat = patOfPats:squeezeJoin()
         local expected = {
            Event(Span(0, 1 / 6), Span(0, 1 / 6), 1),
            Event(Span(1 / 6, 1 / 3), Span(1 / 6, 1 / 3), 2),
            Event(Span(1 / 3, 1 / 2), Span(1 / 3, 1 / 2), 3),
            Event(Span(1 / 2, 2 / 3), Span(1 / 2, 2 / 3), 1),
            Event(Span(2 / 3, 5 / 6), Span(2 / 3, 5 / 6), 2),
            Event(Span(5 / 6, 1), Span(5 / 6, 1), 3),
         }
         assert.are.same(expected, pat(0, 1))
      end
   )
end)
describe("pure", function()
   it("should create Pattern of a single value repeating once per cycle", function()
      local atom = pure(5)
      local expected = {
         Event(Span(0, 1), Span(0, 1), 5, {}, false),
      }
      local actual = atom(0, 1)
      assert.are.same(#expected, #actual)
      assert.are.same(expected, actual)
      expected = {
         Event(Span(0, 1), Span(1 / 2, 1), 5, {}, false),
      }
      actual = atom(1 / 2, 1)
      assert.are.same(#expected, #actual)
      assert.are.same(expected, actual)
   end)
end)
describe("slowcat", function()
   it("should alternate between the patterns in the list, one pattern per cycle", function()
      local pat = slowcat(reify(1), reify(2), reify(3))
      local expected = {
         Event(Span(0, 1), Span(0, 1), 1),
         Event(Span(1, 2), Span(1, 2), 2),
         Event(Span(2, 3), Span(2, 3), 3),
      }
      assert.are.same(expected, pat(0, 3))
   end)
end)
describe("fastcat", function()
   it("should alternate between the patterns in the list, all in one cycle", function()
      local pat = fastcat(reify(1), reify(2), reify(3))
      local expected = {
         Event(Span(0, 1 / 3), Span(0, 1 / 3), 1),
         Event(Span(1 / 3, 2 / 3), Span(1 / 3, 2 / 3), 2),
         Event(Span(2 / 3, 1), Span(2 / 3, 1), 3),
      }
      assert.are.same(expected, pat(0, 1))
   end)
end)

describe("stack", function()
   it("should stack up the pats to be played together", function()
      local pat = stack("bd", "sd", "hh")
      local expected = {
         Event(Span(0, 1), Span(0, 1), "bd"),
         Event(Span(0, 1), Span(0, 1), "sd"),
         Event(Span(0, 1), Span(0, 1), "hh"),
      }
      assert.are.same(expected, pat(0, 1))
   end)
end)

describe("timecat", function()
   it("should return a pattern based one the time-pat 'tuples' passed in", function()
      local pat = timecat({ { 3, fast(4, reify("bd")) }, { 1, fast(8, reify("hh")) } })
      local expected = {
         Event(Span(0, 3 / 16), Span(0, 3 / 16), "bd"),
         Event(Span(3 / 16, 3 / 8), Span(3 / 16, 3 / 8), "bd"),
         Event(Span(3 / 8, 9 / 16), Span(3 / 8, 9 / 16), "bd"),
         Event(Span(9 / 16, 3 / 4), Span(9 / 16, 3 / 4), "bd"),
         Event(Span(3 / 4, 25 / 32), Span(3 / 4, 25 / 32), "hh"),
         Event(Span(25 / 32, 13 / 16), Span(25 / 32, 13 / 16), "hh"),
         Event(Span(13 / 16, 27 / 32), Span(13 / 16, 27 / 32), "hh"),
         Event(Span(27 / 32, 7 / 8), Span(27 / 32, 7 / 8), "hh"),
         Event(Span(7 / 8, 29 / 32), Span(7 / 8, 29 / 32), "hh"),
         Event(Span(29 / 32, 15 / 16), Span(29 / 32, 15 / 16), "hh"),
         Event(Span(15 / 16, 31 / 32), Span(15 / 16, 31 / 32), "hh"),
         Event(Span(31 / 32, 1), Span(31 / 32, 1), "hh"),
      }
      assert.are.same(expected, pat(0, 1))
   end)
end)

describe("fast", function()
   it("should return a pattern whose events are closer together in time", function()
      local pat = fast(2, reify("bd"))
      local expected = {
         Event(Span(0, 0.5), Span(0, 0.5), "bd"),
         Event(Span(0.5, 1), Span(0.5, 1), "bd"),
      }
      assert.are.same(expected, pat(0, 1))
   end)
end)

describe("slow", function()
   it("should return a pattern whose events are closer together in time", function()
      local pat = slow(2, reify("bd sd"))
      local expected = {
         Event(Span(0, 1), Span(0, 1), "bd"),
         Event(Span(1, 2), Span(1, 2), "sd"),
      }
      assert.are.same(expected, pat(0, 2))
   end)
end)

describe("early", function()
   it("should return a pattern whose events are moved backword in time", function()
      local pat = early(0.5, reify("bd sd"))
      local expected = {
         Event(Span(0, 1 / 2), Span(0, 1 / 2), "sd"),
         Event(Span(1 / 2, 1), Span(1 / 2, 1), "bd"),
      }
      assert.are.same(expected, pat(0, 1))
   end)
end)

describe("fastgap", function()
   it("should bring pattern closer together", function()
      local pat = fastgap(4, reify("bd sd"))
      local expected = {
         Event(Span(0, 1 / 8), Span(0, 1 / 8), "bd"),
         Event(Span(1 / 8, 1 / 4), Span(1 / 8, 1 / 4), "sd"),
      }
      assert.are.same(expected, pat(0, 1))
   end)
end)

describe("compress", function()
   it("should bring pattern closer together", function()
      local pat = compress(1 / 4, 3 / 4, reify("bd sd"))
      local expected = {
         Event(Span(1 / 4, 1 / 2), Span(1 / 4, 1 / 2), "bd"),
         Event(Span(1 / 2, 3 / 4), Span(1 / 2, 3 / 4), "sd"),
      }
      assert.are.same(expected, pat(0, 1))
   end)
end)

describe("focus", function()
   it("should bring pattern closer together, but leave no gap, and focus can be bigger than a cycle", function()
      local pat = focus(1 / 4, 3 / 4, reify("bd sd"))
      local expected = {
         Event(Span(0, 1 / 4), Span(0, 1 / 4), "sd"),
         Event(Span(1 / 4, 1 / 2), Span(1 / 4, 1 / 2), "bd"),
         Event(Span(1 / 2, 3 / 4), Span(1 / 2, 3 / 4), "sd"),
         Event(Span(3 / 4, 1), Span(3 / 4, 1), "bd"),
      }
      assert.are.same(expected, pat(0, 1))
   end)
end)

describe("zoom", function()
   it("should play a portion of a pattern", function()
      local pat = zoom(1 / 4, 3 / 4, reify("~ bd sd ~"))
      local expected = {
         Event(Span(0, 1 / 2), Span(0, 1 / 2), "bd"),
         Event(Span(1 / 2, 1), Span(1 / 2, 1), "sd"),
      }
      assert.are.same(expected, pat(0, 1))
   end)
end)

describe("degrade_by", function()
   it("should randomly drop events from a pattern", function()
      local pat = degradeBy(0.75, fast(8, reify("sd")))
      local expected = {
         Event(Span(1 / 8, 1 / 4), Span(1 / 8, 1 / 4), "sd"),
         Event(Span(1 / 2, 5 / 8), Span(1 / 2, 5 / 8), "sd"),
         Event(Span(3 / 4, 7 / 8), Span(3 / 4, 7 / 8), "sd"),
      }
      assert.are.same(expected, pat(0, 1))
   end)
end)
