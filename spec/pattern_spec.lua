local describe = require("busted").describe
local it = require("busted").it
local assert = require("busted").assert

local M = require "modal"
M()
local Span, Event = M.Span, M.Event
local Pattern, reify, pure = M.Pattern, M.reify, M.pure

assert.pat = function(a, b)
   assert.same(a:show(), b:show())
end

describe("new", function()
   it("should initialize with defaults", function()
      local pat = Pattern()
      assert.same({}, pat.query(Span()))
   end)
   it("should create with specified query", function()
      local pat = Pattern(function()
         return { Event() }
      end)
      local Events = pat.query(Span())
      assert.same({ Event() }, Events)
   end)
end)

describe("withValue", function()
   it("should return new pattern with function mapped over Event values on query", function()
      local pat = pure(5)
      local func = function(v)
         return v + 5
      end
      local newPat = pat:withValue(func)
      local expected = { Event(Span(0, 1), Span(0, 1), 10) }
      return assert.same(expected, newPat(0, 1))
   end)
end)

describe("onsetsOnly", function()
   it("should return only Events where the start of the whole equals the start of the part", function()
      local whole1 = Span(1 / 2, 2)
      local part1 = Span(1 / 2, 1)
      local Event1 = Event(whole1, part1, 1, {}, false)
      local whole2 = Span(2 / 3, 3)
      local part2 = Span(5 / 6, 1)
      local Event2 = Event(whole2, part2, 2, {}, false)
      local p = Pattern(function()
         return { Event1, Event2 }
      end)
      local patternWithOnsetsOnly = p:onsetsOnly()
      local actual = patternWithOnsetsOnly(0, 3)
      return assert.same({ Event1 }, actual)
   end)
   it("pure patterns should not behave like continuous signals... they should have discrete onsets", function()
      local p = pure "bd"
      local patternWithOnsetsOnly = p:onsetsOnly()
      local expected = {
         Event(Span(0, 1), Span(0, 1), "bd"),
      }
      local actual = patternWithOnsetsOnly(0, 1)
      assert.same(expected, actual)
      actual = patternWithOnsetsOnly(1 / 16, 1)
      assert.same({}, actual)
   end)
end)

describe("discreteOnly", function()
   it("should return only Events where the start of the whole equals the start of the part", function()
      local ev1 = { Event() }
      local pat = Pattern(function()
         return ev1
      end)
      pat = pat:discreteOnly()
      assert.same({}, pat(0, 1))
      local ev2 = { Event(), Event(Span(), Span(), 1) }
      pat = Pattern(function()
         return ev2
      end)
      pat = pat:discreteOnly()
      local expected = { Event(Span(), Span(), 1) }
      return assert.same(expected, pat(0, 1))
   end)
end)

describe("filterEvents", function()
   return it("should return new pattern with values removed based on filter func", function()
      local pat = slowcat { "bd", "sd", "hh", "mt" }
      local newPat = pat:filterEvents(function(e)
         return e.value == "bd" or e.value == "hh"
      end)
      local expected = {
         Event(Span(0, 1), Span(0, 1), "bd"),
         Event(Span(2, 3), Span(2, 3), "hh"),
      }
      return assert.same(expected, newPat(0, 4))
   end)
end)
describe("withQuerySpan", function()
   it("should return new pattern with that modifies query Span with function when queried", function()
      local pat = pure(5)
      local func = function(span)
         return Span(span.start + 0.5, span.stop + 0.5)
      end
      local newPat = pat:withQuerySpan(func)
      local expected = {
         Event(Span(0, 1), Span(0.5, 1), 5),
         Event(Span(1, 2), Span(1, 1.5), 5),
      }
      assert.same(expected, newPat(0, 1))
   end)
end)

describe("splitQueries", function()
   it("should break a query that Spans multiple cycles into multiple queries each Spanning one cycle", function()
      local query = function(span)
         return { Event(span, span, "a") }
      end
      local pat = Pattern(query)
      local splitPat = pat:splitQueries()
      local expectedPat = { Event(Span(0, 2), Span(0, 2), "a") }
      local expectedSplit = {
         Event(Span(0, 1), Span(0, 1), "a"),
         Event(Span(1, 2), Span(1, 2), "a"),
      }
      assert.same(expectedPat, pat(0, 2))
      assert.same(expectedSplit, splitPat(0, 2))
   end)
end)

describe("withQueryTime", function()
   it(
      "should return new pattern whose query function will pass the query timeSpan through a function before mapping it to Events",
      function()
         local pat = pure(5)
         local add1 = function(other)
            return other + 1
         end
         local newPat = pat:withQueryTime(add1)
         local expected = {
            Event(Span(1, 2), Span(1, 2), 5),
         }
         assert.same(expected, newPat(0, 1))
      end
   )
end)

describe("withEventTime", function()
   it("should return new pattern with function mapped over Event times", function()
      local pat = pure(5)
      local func = function(time)
         return time + 0.5
      end
      local newPat = pat:withEventTime(func)
      local expected = {
         Event(Span(0.5, 1.5), Span(0.5, 1.5), 5),
      }
      assert.same(expected, newPat(0, 1))
   end)
end)

describe("appLeft", function()
   it("should take structure from left and appliy f", function()
      local add = function(a)
         return function(b)
            return a + b
         end
      end
      local left = reify({ 1, 2 }):fmap(add)
      local right = reify { 4, 5, 6 }
      local expected = {
         Event(Span(0, 1 / 2), Span(0, 1 / 3), 5),
         Event(Span(0, 1 / 2), Span(1 / 3, 1 / 2), 6),
         Event(Span(1 / 2, 1), Span(1 / 2, 2 / 3), 7),
         Event(Span(1 / 2, 1), Span(2 / 3, 1), 8),
      }
      local pat = left:appLeft(right)
      assert.same(expected, pat(0, 1))
   end)
end)
describe("appRight", function()
   it("should take structure from right and appliy f", function()
      local add = function(a)
         return function(b)
            return a + b
         end
      end
      local left = reify({ 1, 2 }):fmap(add)
      local right = reify { 4, 5, 6 }
      local expected = {
         Event(Span(0, 1 / 3), Span(0, 1 / 3), 5),
         Event(Span(1 / 3, 2 / 3), Span(1 / 3, 1 / 2), 6),
         Event(Span(1 / 3, 2 / 3), Span(1 / 2, 2 / 3), 7),
         Event(Span(2 / 3, 1), Span(2 / 3, 1), 8),
      }
      local pat = left:appRight(right)
      assert.same(expected, pat(0, 1))
   end)
end)

describe("appRight", function()
   it("should take structure from right and appliy f", function()
      local add = function(a)
         return function(b)
            return a + b
         end
      end
      local left = reify({ 1, 2 }):fmap(add)
      local right = reify { 4, 5, 6 }
      local expected = {
         Event(Span(0, 1 / 3), Span(0, 1 / 3), 5),
         Event(Span(1 / 3, 2 / 3), Span(1 / 3, 1 / 2), 6),
         Event(Span(1 / 3, 2 / 3), Span(1 / 2, 2 / 3), 7),
         Event(Span(2 / 3, 1), Span(2 / 3, 1), 8),
      }
      local pat = left:appRight(right)
      assert.same(expected, pat(0, 1))
   end)
end)

describe("appBoth", function()
   it("should take structure from both sides and appliy f", function()
      local add = function(a)
         return function(b)
            return a + b
         end
      end
      local left = reify({ 1, 2 }):fmap(add)
      local right = reify { 4, 5, 6 }
      local expected = {
         Event(Span(0, 1 / 3), Span(0, 1 / 3), 5),
         Event(Span(1 / 3, 1 / 2), Span(1 / 3, 1 / 2), 6),
         Event(Span(1 / 2, 2 / 3), Span(1 / 2, 2 / 3), 7),
         Event(Span(2 / 3, 1), Span(2 / 3, 1), 8),
      }
      local pat = left:appBoth(right)
      assert.same(expected, pat(0, 1))
   end)
end)

describe("outerJoin", function()
   it(
      "it should convert a pattern of patterns into a single pattern with time structure coming from the outer pattern",
      function()
         local patOfPats = pure(fastcat { "a", "b" })
         local pat = patOfPats:outerJoin()
         local expected = {
            Event(Span(0, 1), Span(0, 1 / 2), "a"),
            Event(Span(0, 1), Span(1 / 2, 1), "b"),
         }
         assert.same(expected, pat(0, 1))
      end
   )
end)

describe("squeezeJoin", function()
   it(
      "it should convert a pattern of patterns into a single pattern, takes whole cycles of the inner pattern to fit each Event in the outer pattern.\n ",
      function()
         local patOfPats = fastcat { fastcat { 1, 2, 3 }, fastcat { 1, 2, 3 } }
         local pat = patOfPats:squeezeJoin()
         local expected = {
            Event(Span(0, 1 / 6), Span(0, 1 / 6), 1),
            Event(Span(1 / 6, 1 / 3), Span(1 / 6, 1 / 3), 2),
            Event(Span(1 / 3, 1 / 2), Span(1 / 3, 1 / 2), 3),
            Event(Span(1 / 2, 2 / 3), Span(1 / 2, 2 / 3), 1),
            Event(Span(2 / 3, 5 / 6), Span(2 / 3, 5 / 6), 2),
            Event(Span(5 / 6, 1), Span(5 / 6, 1), 3),
         }
         assert.same(expected, pat(0, 1))
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
      assert.same(#expected, #actual)
      assert.same(expected, actual)
      expected = {
         Event(Span(0, 1), Span(1 / 2, 1), 5, {}, false),
      }
      actual = atom(1 / 2, 1)
      assert.same(#expected, #actual)
      assert.same(expected, actual)
   end)
end)

describe("slowcat", function()
   it("should alternate between the patterns in the list, one pattern per cycle", function()
      local pat = slowcat { 1, 2, 3 }
      local expected = {
         Event(Span(0, 1), Span(0, 1), 1),
         Event(Span(1, 2), Span(1, 2), 2),
         Event(Span(2, 3), Span(2, 3), 3),
      }
      assert.same(expected, pat(0, 3))
   end)
end)

describe("fastcat", function()
   it("should alternate between the patterns in the list, all in one cycle", function()
      local pat = fastcat { 1, 2, 3 }
      local expected = {
         Event(Span(0, 1 / 3), Span(0, 1 / 3), 1),
         Event(Span(1 / 3, 2 / 3), Span(1 / 3, 2 / 3), 2),
         Event(Span(2 / 3, 1), Span(2 / 3, 1), 3),
      }
      assert.same(expected, pat(0, 1))
   end)
end)

describe("randcat", function()
   it("should randomly alternate between the patterns in the list, all in one cycle", function()
      local pat = randcat { 1, 2, 3 }
      assert.same(pure(1)(0, 1), pat(0, 1))
      assert.same(pure(2)(1, 2), pat(1, 2))
      assert.same(pure(2)(2, 3), pat(2, 3))
   end)
end)

describe("stack", function()
   it("should stack up the pats to be played together", function()
      local pat = stack { pure "bd", pure "sd", pure "hh" }
      local expected = {
         Event(Span(0, 1), Span(0, 1), "bd"),
         Event(Span(0, 1), Span(0, 1), "sd"),
         Event(Span(0, 1), Span(0, 1), "hh"),
      }
      assert.same(expected, pat(0, 1))
   end)
end)

describe("timecat", function()
   it("should return a pattern based one the time-pat 'tuples' passed in", function()
      local pat = timecat { 3, fast(4, pure "bd"), 1, fast(8, pure "hh") }
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
      assert.same(expected, pat(0, 1))
   end)
end)

describe("polymeter", function()
   it("should stack up pats with right time compress ratios", function()
      local pat = polymeter(2, { reify { "bd", "sd" }, reify { "1", "2", "3" } })
      local expected1 = stack { reify { "bd", "sd" }, reify { 1, 2 } }
      assert.same(expected1(0, 1), pat(0, 1))
      local expected2 = stack { reify { "bd", "sd" }, reify { 3, 1 } }
      assert.same(expected2(1, 2), pat(1, 2))
   end)
end)

describe("fast", function()
   it("should return a pattern whose Events closer together in time", function()
      local pat = fast(2, pure "bd")
      local expected = {
         Event(Span(0, 0.5), Span(0, 0.5), "bd"),
         Event(Span(0.5, 1), Span(0.5, 1), "bd"),
      }
      assert.same(expected, pat(0, 1))
   end)
   it("should return silence with factor 0", function()
      local pat = fast(0, "sd")
      assert.same({}, pat(0, 1))
   end)
   it("should return reversed with negative factor", function()
      local pat = fast(-2, "bd sd")
      local expected = rev(fast(2, "bd sd"))
      assert.same(expected(0, 1), pat(0, 1))
   end)
end)

describe("slow", function()
   it("should return a pattern whose Events closer together in time", function()
      local pat = slow(2, reify { "bd", "sd" })
      local expected = {
         Event(Span(0, 1), Span(0, 1), "bd"),
         Event(Span(1, 2), Span(1, 2), "sd"),
      }
      assert.same(expected, pat(0, 2))
   end)
end)

describe("early", function()
   it("should return a pattern whose Events moved backword in time", function()
      local pat = early(0.5, reify { "bd", "sd" })
      local expected = reify { "sd", "bd" }
      assert.pat(expected, pat)
   end)
end)

describe("fastgap", function()
   it("should bring pattern closer together", function()
      local pat = fastgap(4, reify { "bd", "sd" })
      local expected = timecat { 1, "bd", 1, "sd", 6, silence }
      assert.pat(expected, pat)
   end)
end)

describe("compress", function()
   it("should bring pattern closer together", function()
      local pat = compress(0.25, 0.75, fastcat { "bd", "sd" })
      local expected = fastcat { silence, "bd", "sd", silence }
      assert.pat(expected, pat)
   end)
end)

describe("focus", function()
   it("should bring pattern closer together, but leave no gap, and focus can be bigger than a cycle", function()
      local pat = focus(1 / 4, 3 / 4, reify { "bd", "sd" })
      local expected = fastcat { "sd", "bd", "sd", "bd" }
      assert.pat(expected, pat)
   end)
end)

describe("zoom", function()
   it("should play a portion of a pattern", function()
      local pat = zoom(1 / 4, 3 / 4, reify { "x", "bd", "sd", "x" })
      local expected = fastcat { "bd", "sd" }
      assert.pat(expected, pat)
   end)
end)

describe("degrade_by", function()
   it("should randomly drop Events from a pattern", function()
      local pat = degradeBy(0.75, fast(8, "sd"))
      local expected = {
         Event(Span(1 / 8, 1 / 4), Span(1 / 8, 1 / 4), "sd"),
         Event(Span(1 / 2, 5 / 8), Span(1 / 2, 5 / 8), "sd"),
         Event(Span(3 / 4, 7 / 8), Span(3 / 4, 7 / 8), "sd"),
      }
      assert.same(expected, pat(0, 1))
   end)
end)

describe("run", function()
   it("should gen 0 - n numbers", function()
      local pat = run(3)
      local expected = {
         Event(Span(0, 1 / 3), Span(0, 1 / 3), 0),
         Event(Span(1 / 3, 2 / 3), Span(1 / 3, 2 / 3), 1),
         Event(Span(2 / 3, 1), Span(2 / 3, 1), 2),
      }
      assert.same(expected, pat(0, 1))
   end)
end)

describe("run", function()
   it("should gen 0 - n numbers", function()
      local pat = scan(3)
      for i = 1, 3 do
         assert.same(run(i)(i - 1, i), pat(i - 1, i))
      end
   end)
end)

describe("euclid", function()
   it("shoudl gen euclid pats", function()
      local pat = euclidRot(3, 8, 1, "bd")
      local expected = struct(bjork(3, 8, 1), "bd")
      assert.pat(expected, pat)
   end)
end)

describe("off", function()
   -- TODO:
   it("should offset applying f", function()
      local inc1 = function(a)
         return a + 1
      end
      local pat = off(0.5, inc1, 1)
      local expected = {
         Event(Span(-0.5, 0.5), Span(0, 0.5), 2),
         Event(Span(0.5, 1.5), Span(0.5, 1), 2),
         Event(Span(0, 1), Span(0, 1), 1),
      }
      assert.same(expected, pat(0, 1))
   end)

   it("should take string lambda that gets lib funcs env", function()
      --       Passed in:
      -- (table: 0x559b9be4fd20) {
      --  *[1] = (0/1-1/2)-1/1 | 2
      --   [2] = 0/1-(1/2-1/1) | 2
      --   [3] = (0/1-1/1) | 1 }
      -- Expected:
      -- (table: 0x559b9be4b780) {
      --  *[1] = -1/2-(0/1-1/2) | 2
      --   [2] = (1/2-1/1)-3/2 | 2
      --   [3] = (0/1-1/1) | 1 }
      -- Tidal
      -- (0>1/2)|2
      -- (0>1)|1
      -- (1/2>1)|2
      -- local pat = off(0.5, "(+ 1)", 1)
      -- local expected = {
      --    Event(Span(-0.5, 0.5), Span(0, 0.5), 2),
      --    Event(Span(0.5, 1.5), Span(0.5, 1), 2),
      --    Event(Span(0, 1), Span(0, 1), 1),
      -- }
      --
      -- assert.same(expected, pat(0, 1))
   end)
end)

describe("every", function()
   it("should apply f every n cycles", function()
      local inc1 = function(a)
         return a + 1
      end
      local pat = every(3, inc1, 1)
      local expected = slowcat { 2, 1, 1 }
      assert.pat(expected, pat)
   end)

   it("should take pattern of functions as second param", function()
      local pat = every(3, stack { fast(2), reify "(+ 1)" }, 1)
      local expected = stack { slowcat { fast(2, 1), 1, 1 }, slowcat { 2, 1, 1 } }
      assert.pat(expected, pat)
   end)

   it("should take string lambda that gets lib funcs env", function()
      local pat = every(3, "fast 2", 1)
      local expected = slowcat { fast(2, 1), 1, 1 }
      assert.pat(expected, pat)
   end)
   -- TODO:
   -- it("should take mini-notation of functions", function()
   --    local pat = every(3, "[(+ 1), (fast 2)]", 1)
   --    local expected = stack { slowcat { fast(2, 1), 1, 1 }, slowcat { 2, 1, 1 } }
   --    assert.pat(expected, pat)
   -- end)
end)

describe("scale", function()
   it("should `quantise` notes in scale", function()
      -- gong : { 0, 2, 4, 7, 9 }
      local pat = note("1 2 3"):scale "gong"
      local expected = note "2 4 7"
      assert.pat(expected, pat)
   end)
   it("should do mod", function()
      local pat = note("5 6 7"):scale "gong"
      local expected = note "12 14 16" -- 0, 2, 4 + 12
      assert.pat(expected, pat)
   end)
end)

describe("struct", function()
   it("should give bool struct to pat", function()
      assert.pat(reify { 1, 1 }, M.struct({ true, true }, 1))
      assert.pat(reify { 1, silence, 1 }, M.struct({ true, false, true }, 1))
   end)
end)

describe("ply", function()
   it("should repeat every element in the cycle with give times", function()
      assert.pat(reify { 1, 1, 1, 2, 2, 2 }, ply(3, reify { 1, 2 }))
   end)
end)

describe("Tidal operators", function()
   it("", function() end)
   -- it("register ops as pattern methods", function()
   --    local pat = n(1)["|>"](s "bd")
   --    local expected = op["|>"](n(1), s "bd")
   --    assert.pat(expected, pat)
   -- end)
end)

describe("layer", function()
   it("", function()
      local inc1 = function(x)
         return x + 1
      end
      assert.pat(stack { 2, 1 }, layer({ inc1, id }, 1))
   end)
end)

describe("juxBy", function()
   it("", function()
      assert.pat(stack { s("bd"):pan(0.25), s("bd"):fast(2):pan(0.75) }, juxBy(0.5, fast(2), s "bd"))
   end)
end)

describe("striate", function()
   it("", function()
      local pat = striate(2, s "bd")
      local expected =
         fastcat { reify { ["begin"] = 0, ["end"] = 0.5, s = "bd" }, { ["begin"] = 0.5, ["end"] = 1, s = "bd" } }
      assert.pat(expected, pat)
   end)
end)

describe("chop", function()
   it("", function()
      local pat = chop(2, s "bd sd")
      local expected = fastcat {
         reify { ["begin"] = 0, ["end"] = 0.5, s = "bd" },
         reify { ["begin"] = 0.5, ["end"] = 1, s = "bd" },
         reify { ["begin"] = 0, ["end"] = 0.5, s = "sd" },
         reify { ["begin"] = 0.5, ["end"] = 1, s = "sd" },
      }
      assert.pat(expected, pat)
   end)
end)

describe("loopAt", function()
   it("", function()
      local pat = s("bd sd"):chop(2):loopAt(2)
      local expected = fastcat {
         reify { ["begin"] = 0, ["end"] = 0.5, s = "bd", speed = 0.5, unit = "c" },
         reify { ["begin"] = 0.5, ["end"] = 1, s = "bd", speed = 0.5, unit = "c" },
      }
      assert.pat(expected, pat)
   end)
end)

-- describe("fit", function()
--    it("", function()
--       local pat = fit(s "bd sd")
--       local expected = fastcat {
--          reify { speed = 2, unit = "c", s = "bd" },
--          reify { speed = 2, unit = "c", s = "sd" },
--       }
--       assert.pat(expected, pat)
--    end)
-- end)

-- describe("legato", function()
--    it("", function()
--       local pat = legato(2, "bd")
--       local expected = fastcat {
--          reify { speed = 2, unit = "c", s = "bd" },
--          reify { speed = 2, unit = "c", s = "sd" },
--       }
--       assert.pat(expected, pat)
--    end)
-- end)
--

describe("slice", function()
   it("", function()
      local pat = slice(8, " 0 1", "bd")
      local expected = fastcat {
         reify { ["begin"] = 0, ["end"] = 1 / 8, s = "bd" },
         reify { ["begin"] = 1 / 8, ["end"] = 1 / 4, s = "bd" },
      }
      assert.pat(expected, pat)
   end)
end)
