local Span, State, Event
do
   local _obj_0 = require("modal.types")
   Span, State, Event = _obj_0.Span, _obj_0.State, _obj_0.Event
end
local Pattern, mini, pure, stack, slowcat, fastcat, timecat, fast, slow, early, late, inside, outside, fastgap, compress, zoom, focus, degradeBy, striate, chop, slice, splice, iter
do
   local _obj_0 = require("modal.pattern")
   Pattern, mini, pure, stack, slowcat, fastcat, timecat, fast, slow, early, late, inside, outside, fastgap, compress, zoom, focus, degradeBy, striate, chop, slice, splice, iter =
      _obj_0.Pattern,
      _obj_0.mini,
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
return describe("Pattern", function()
   describe("new", function()
      it("should initialize with defaults", function()
         local pat = Pattern()
         return assert.are.same({}, pat:query(State()))
      end)
      return it("should create with specified query", function()
         local pat = Pattern(function()
            return {
               Event(),
            }
         end)
         local events = pat:query(State())
         return assert.are.same({
            Event(),
         }, events)
      end)
   end)
   describe("withValue", function()
      return it("should return new pattern with function mapped over event values on query", function()
         local pat = pure(5)
         local func
         func = function(v)
            return v + 5
         end
         local newPat = withValue(pat, func)
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
         local patternWithOnsetsOnly = onsetsOnly(p)
         local actual = patternWithOnsetsOnly(0, 3)
         return assert.are.same({
            event1,
         }, actual)
      end)
      return it(
         "pure patterns should not behave like continuous signals... they should have discrete onsets",
         function()
            local p = pure("bd")
            local patternWithOnsetsOnly = onsetsOnly(p)
            local expected = {
               Event(Span(0, 1), Span(0, 1), "bd"),
            }
            local actual = patternWithOnsetsOnly(0, 1)
            assert.are.same(expected, actual)
            actual = patternWithOnsetsOnly(1 / 16, 1)
            return assert.are.same({}, actual)
         end
      )
   end)
   describe("discreteOnly", function()
      return it("should return only events where the start of the whole equals the start of the part", function()
         local ev1 = {
            Event(),
         }
         local pat = Pattern(function(state)
            return ev1
         end)
         pat = discreteOnly(pat)
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
         local pat = slowcat("bd", "sd", "hh", "mt")
         local newPat = filterEvents(pat, function(e)
            return e.value == "bd" or e.value == "hh"
         end)
         local expected = {
            Event(Span(0, 1), Span(0, 1), "bd"),
            Event(Span(2, 3), Span(2, 3), "hh"),
         }
         return assert.are.same(expected, newPat(0, 4))
      end)
   end)
   describe("pure", function()
      return it("should create Pattern of a single value repeating once per cycle", function()
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
         return assert.are.same(expected, actual)
      end)
   end)
   describe("slowcat", function()
      return it("should alternate between the patterns in the list, one pattern per cycle", function()
         local pat = slowcat(1, 2, 3)
         local expected = {
            Event(Span(0, 1), Span(0, 1), 1),
            Event(Span(1, 2), Span(1, 2), 2),
            Event(Span(2, 3), Span(2, 3), 3),
         }
         return assert.are.same(expected, pat(0, 3))
      end)
   end)
   describe("fastcat", function()
      return it("should alternate between the patterns in the list, all in one cycle", function()
         local pat = fastcat(1, 2, 3)
         local expected = {
            Event(Span(0, 1 / 3), Span(0, 1 / 3), 1),
            Event(Span(1 / 3, 2 / 3), Span(1 / 3, 2 / 3), 2),
            Event(Span(2 / 3, 1), Span(2 / 3, 1), 3),
         }
         return assert.are.same(expected, pat(0, 1))
      end)
   end)
   describe("stack", function()
      return it("should stack up the pats to be played together", function()
         local pat = stack("bd", "sd", "hh")
         local expected = {
            Event(Span(0, 1), Span(0, 1), "bd"),
            Event(Span(0, 1), Span(0, 1), "sd"),
            Event(Span(0, 1), Span(0, 1), "hh"),
         }
         return assert.are.same(expected, pat(0, 1))
      end)
   end)
   describe("timecat", function()
      return it("should return a pattern based one the time-pat 'tuples' passed in", function()
         local pat = timecat({
            {
               3,
               fast(4, mini("bd")),
            },
            {
               1,
               fast(8, mini("hh")),
            },
         })
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
         return assert.are.same(expected, pat(0, 1))
      end)
   end)
   describe("fast", function()
      return it("should return a pattern whose events are closer together in time", function()
         local pat = fast(2, mini("bd"))
         local expected = {
            Event(Span(0, 0.5), Span(0, 0.5), "bd"),
            Event(Span(0.5, 1), Span(0.5, 1), "bd"),
         }
         return assert.are.same(expected, pat(0, 1))
      end)
   end)
   describe("slow", function()
      return it("should return a pattern whose events are closer together in time", function()
         local pat = slow(2, mini("bd sd"))
         local expected = {
            Event(Span(0, 1), Span(0, 1), "bd"),
            Event(Span(1, 2), Span(1, 2), "sd"),
         }
         return assert.are.same(expected, pat(0, 2))
      end)
   end)
   describe("early", function()
      return it("should return a pattern whose events are moved backword in time", function()
         local pat = early(0.5, mini("bd sd"))
         local expected = {
            Event(Span(0, 1 / 2), Span(0, 1 / 2), "sd"),
            Event(Span(1 / 2, 1), Span(1 / 2, 1), "bd"),
         }
         return assert.are.same(expected, pat(0, 1))
      end)
   end)
   describe("fastgap", function()
      return it("should bring pattern closer together", function()
         local pat = fastgap(4, mini("bd sd"))
         local expected = {
            Event(Span(0, 1 / 8), Span(0, 1 / 8), "bd"),
            Event(Span(1 / 8, 1 / 4), Span(1 / 8, 1 / 4), "sd"),
         }
         return assert.are.same(expected, pat(0, 1))
      end)
   end)
   describe("compress", function()
      return it("should bring pattern closer together", function()
         local pat = compress(1 / 4, 3 / 4, mini("bd sd"))
         local expected = {
            Event(Span(1 / 4, 1 / 2), Span(1 / 4, 1 / 2), "bd"),
            Event(Span(1 / 2, 3 / 4), Span(1 / 2, 3 / 4), "sd"),
         }
         return assert.are.same(expected, pat(0, 1))
      end)
   end)
   describe("focus", function()
      return it(
         "should bring pattern closer together, but leave no gap, and focus can be bigger than a cycle",
         function()
            local pat = focus(1 / 4, 3 / 4, mini("bd sd"))
            local expected = {
               Event(Span(0, 1 / 4), Span(0, 1 / 4), "sd"),
               Event(Span(1 / 4, 1 / 2), Span(1 / 4, 1 / 2), "bd"),
               Event(Span(1 / 2, 3 / 4), Span(1 / 2, 3 / 4), "sd"),
               Event(Span(3 / 4, 1), Span(3 / 4, 1), "bd"),
            }
         end
      )
   end)
   describe("zoom", function()
      return it("should play a portion of a pattern", function()
         local pat = zoom(1 / 4, 3 / 4, mini("~ bd sd ~"))
         local expected = {
            Event(Span(0, 1 / 2), Span(0, 1 / 2), "bd"),
            Event(Span(1 / 2, 1), Span(1 / 2, 1), "sd"),
         }
         return assert.are.same(expected, pat(0, 1))
      end)
   end)
   return describe("degrade_by", function()
      return it("should randomly drop events from a pattern", function()
         local pat = degradeBy(0.75, fast(8, mini("sd")))
         local expected = {
            Event(Span(1 / 8, 1 / 4), Span(1 / 8, 1 / 4), "sd"),
            Event(Span(1 / 2, 5 / 8), Span(1 / 2, 5 / 8), "sd"),
            Event(Span(3 / 4, 7 / 8), Span(3 / 4, 7 / 8), "sd"),
         }
         return assert.are.same(expected, pat(0, 1))
      end)
   end)
end)
