local read = require("modal.maxi").read
local eval = require("modal.maxi").eval
local to_lua = require("modal.maxi").to_lua
local mlc = require("metalua.compiler").new()

local describe = require("busted").describe
local it = require("busted").it
local assert = require("busted").assert

Id = function(a)
   return { tag = "Id", a }
end

Str = function(a)
   return { tag = "String", a }
end

Num = function(a)
   return { tag = "Number", a }
end

Pure = function(a)
   return { tag = "Call", Id("pure"), a }
end

describe("symb", function()
   it("should parse steps to lua String or Id", function()
      assert.same([["hello"]], to_lua("hello"))
   end)
   it("should parse steps to lua String or Id", function()
      assert.same([[42]], to_lua("42"))
   end)
end)

describe("set", function()
   it("should parse set to lua var set", function()
      assert.same("a = 1", to_lua([[a = 1]]))
   end)
   it("should parse set a sexp to var", function()
      assert.same("a = fast(2, 1)", to_lua([[a = (fast 2 1)]]))
   end)
   it("should parse set a mini-notation to var", function()
      assert.same('a = fastcat("bd", "sd")', to_lua("a = [bd sd]"))
   end)
end)

describe("subcycle", function()
   it("should parse mini subcycle as a first class", function()
      assert.same([[fastcat("bd")]], to_lua("[bd]"))
      assert.same([[fastcat("bd", "sd")]], to_lua("[bd sd]"))
   end)
end)

describe("stack", function()
   it("should parse mini stack as a first class", function()
      assert.same([[stack("bd", "sd")]], to_lua("[bd, sd]"))
   end)
end)

describe("slow_seq", function()
   it("should parse mini slow_seq as a first class", function()
      assert.same([[slowcat("bd")]], to_lua("<bd>"))
      assert.same([[slowcat("bd", "sd")]], to_lua("<bd sd>"))
   end)
end)
require("modal")()

describe("ops", function()
   it("should parse mini ops as a first class", function()
      assert.same([[fast(2, "bd")]], to_lua("bd*2"))
      assert.same([[euclid(3, 8, 0, "bd")]], to_lua("bd(3,8)"))
      assert.same([[degradeBy(0.5, "bd")]], to_lua("bd?", "degradeBy"))
      assert.same([[fastcat("bd", "bd", "sd")]], to_lua("[bd! sd]"))
      -- assert.same(timecat({ { 2, pure("bd") }, { 1, pure("sd") } }), eval("[bd@2 sd]"))
      assert.same(timecat({ { 2, "bd" }, { 1, "sd" } }), eval("[bd@2 sd]"))
   end)
end)

describe("list(p)", function()
   it("should parse sexp as a first class", function()
      assert.same(3, eval("(+ 1 2)"))
      assert.same(3, eval("(1 + 2)"))
      assert.same(-1, eval("(- 1 2)"))
   end)
   it("should parse sexp func call as a first class", function()
      assert.same("fast(1, 2)", to_lua("(fast 1 2)"))
      assert.same("fast(1, pure(2))", to_lua("(fast 1 (pure 2))"))
   end)
end)
