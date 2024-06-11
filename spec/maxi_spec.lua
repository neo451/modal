local M = require "modal"
local maxi = require "modal.maxi"
local describe = require("busted").describe
local it = require("busted").it
local assert = require("busted").assert
-- TODO: test toplevel func calls

-- describe("symb", function()
--    it("should parse steps to lua String or Id", function()
--       assert.same([["hello"]], to_lua("hello"))
--    end)
--    it("should parse steps to lua String or Id", function()
--       assert.same([[42]], to_lua("42"))
--    end)
-- end)
--
-- TODO: should everything just be default mini?? quote to get literal???
describe("set", function()
   -- it("should parse set to lua var set in only top level????", function()
   --    assert.same("a = 1; ", to_str [[ a = 1 ]])
   -- end)
   -- it("should parse set a func call to var", function()
   --    assert.same("a = fast(pure2(2), pure2(1)); ", to_str " a = $ fast 2 1 ")
   -- end)
   -- it("should parse set a sexp to var", function()
   --    assert.same("a = fast(2, 1); ", to_str " a = (fast 2 1) ")
   -- end)
   -- it("should parse set a mini-notation to var", function()
   --    assert.same([[a = fastcat({pure("bd"),pure("sd")}); ]], to_str "a = [bd sd]")
   -- end)
end)

-- describe("type checks", function()
--    it("should gen warnings and errors based on type signature", function()
--       -- assert.has_error(M.run "sd")
--       assert.has_error(eval_top "run sd")
--    end)
-- end)
-- local eval = maxi(M, false)
local eval = function(str)
   return M.mini(str)()
end

local eval_top = function(str)
   return maxi(M, true)(str)()
end

describe("step", function()
   it("numbers", function()
      -- assert.same(M.pure(-1), eval "-1")
      assert.same(-1, eval "-1")
   end)
end)

describe("slice", function()
   it("should parse mini slice as a first class", function()
      assert.same("bd", eval "bd")
   end)
end)

describe("subcycle", function()
   it("should parse mini subcycle as a first class", function()
      assert.same(M.fastFromList { "bd" }, eval "[bd]")
      assert.same(M.fastFromList { "bd", "sd" }, eval "[bd sd]")
   end)
end)

describe("stack", function()
   it("should parse mini stack as a first class", function()
      assert.same(M.stack { M.pure "bd", M.pure "sd" }, eval "[bd, sd]")
      assert.same(M.stack { M.fastFromList { "bd", "bd" }, M.fastFromList { "sd", "sd" } }, eval "[bd bd, sd sd]")
   end)
end)

describe("slow_seq", function()
   it("should parse mini slow_seq as a first class", function()
      assert.same(M.fromList { "bd" }, eval "<bd>")
      assert.same(M.fromList { "bd", "sd" }, eval "<bd sd>")
      assert.same(M.fromList { 0, 1, 2, 3, 4 }, eval "<0 .. 4>")
   end)
end)

describe("polymeter", function()
   it("should parse mini polymeter as a first class", function()
      assert.same(M.fastFromList { "bd", "sd", "hh" }, eval "{bd sd hh}")
      assert.same(M.fastFromList { "bd", "sd", "hh" }, eval "{bd sd hh}%3")
      assert.same(M.fastFromList { "bd", "sd" }, eval "{bd sd hh}%2")
      assert.same(M.stack { M.fastFromList { "bd", "sd" }, M.fastFromList { 1, 2 } }, eval "{bd sd hh, 1 2 3 4}%2")
   end)
end)

describe("choose", function()
   it("should parse mini choose as first class", function()
      assert.same(M.randcat(M.pure "bd", M.pure "sd", M.pure "cp"), eval "[bd | sd | cp]")
      assert.same(M.randcat(M.fastFromList { "bd", "sd" }, M.pure "sd", M.pure "cp"), eval "[bd sd | sd | cp]")
   end)
end)

describe("ops", function()
   it("should parse mini ops as a first class", function()
      assert.same(M.fast(2, "bd"), eval "bd*2")
      assert.same(M.euclid(3, 8, 0, "bd"), eval "bd(3,8)")
      assert.same(M.degradeBy(0.5, "bd"), eval("bd?", "degradeBy"))
      assert.same(M.fastFromList { "bd", "bd", "sd" }, eval "[bd! sd]")
      assert.same(M.pure { 0.3, 0.5, 2 }, eval "0.3:0.5:2")
   end)
   it("weight", function()
      assert.same(M.timecat { 2, "bd", 1, "sd" }, eval "[bd@2 sd]")
      assert.same(M.timecat { 3, "bd", 2, "sd" }, eval "[bd __ sd _]")
      assert.same(M.arrange { 2, "bd", 1, "sd" }, eval "<bd@2 sd>")
   end)
end)

describe("tidal ops", function()
   it("should parse tidal ops as a first class", function()
      assert.same(M.note(3), eval_top [[note 2 +| note 1]])
      assert.same(M.pure { s = "bd", room = "0.2" }, eval_top [[s bd |> room 0.2]])
      -- TODO:
      -- assert.same(M.pure { s = "bd", room = "0.2" }, eval_top [[s bd >|| room 0.2]])
   end)
end)

-- describe("list(p)", function()
--    it("should parse sexp as a first class", function()
--       assert.same(3, eval("(+ 1 2)"))
--       assert.same(3, eval("(1 + 2)"))
--       assert.same(-1, eval("(- 1 2)"))
--    end)
--    it("should parse sexp func call as a first class", function()
--       assert.same("fast(1, 2)", to_lua("(fast 1 2)"))
--       assert.same("fast(1, pure(2))", to_lua("(fast 1 (pure 2))"))
--    end)
-- end)
