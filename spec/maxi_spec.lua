local M = require"modal"
local maxi = require("modal.maxi").maxi
local to_lua = require("modal.maxi").to_lua
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
-- local eval_top = maxi(M, true)
local to_str = to_lua(M, true)
describe("set", function()
   it("should parse set to lua var set in only top level????", function()
      assert.same("a = 1; ", to_str([[ a = 1 ]]))
   end)
   it("should parse set a sexp to var", function() 
      assert.same("a = fast(2, 1); ", to_str(" a = (fast 2 1) "))
   end)
   it("should parse set a mini-notation to var", function()
      assert.same([[a = fastcat({pure("bd"),pure("sd")}); ]], to_str("a = [bd sd]"))
   end)
end)

local eval = maxi(M, false)

describe("slice", function()
   it("should parse mini slice as a first class", function()
      assert.same(M.pure"bd", eval("bd"))
   end)
end)


describe("subcycle", function()
   it("should parse mini subcycle as a first class", function()
      assert.same(M.fastFromList({"bd"}), eval("[bd]"))
      assert.same(M.fastFromList({"bd", "sd"}), eval("[bd sd]"))
   end)
end)

describe("stack", function()
   it("should parse mini stack as a first class", function()
      assert.same(M.stackFromList{"bd", "sd"}, eval("[bd, sd]"))
   end)
end)

describe("slow_seq", function()
   it("should parse mini slow_seq as a first class", function()
      assert.same(M.fromList{"bd"}, eval("<bd>"))
      assert.same(M.fromList{"bd", "sd"}, eval("<bd sd>"))
   end)
end)

describe("ops", function()
   it("should parse mini ops as a first class", function()
      assert.same(M.fast(2,"bd"), eval("bd*2"))
      assert.same(M.euclid(3,8,0, "bd"), eval("bd(3,8)"))
      assert.same(M.degradeBy(0.5, "bd"), eval("bd?", "degradeBy"))
      assert.same(M.fastFromList{"bd", "bd", "sd"}, eval("[bd! sd]"))
      assert.same(M.timecat({ 2, M.pure"bd", 1, M.pure"sd" }), eval("[bd@2 sd]"))
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
