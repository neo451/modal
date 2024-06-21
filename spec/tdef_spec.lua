local describe = require("busted").describe
local M = require "modal.typedef"
local it = require("busted").it
local assert = require("busted").assert

describe("id", function()
   it("should return elem", function()
      assert.same({ { "a" }, ret = { "a" }, source = "a -> a" }, M "a -> a")
   end)
   it("should return elem", function()
      assert.same(
         { { "Time" }, ret = { constructor = "Pattern", "a" }, source = "Time -> Pattern a" },
         M "Time -> Pattern a"
      )
   end)
end)

describe("table", function()
   it("should return elem with table tag", function()
      assert.same({ { "a", istable = true }, ret = { "a" }, source = "[a] -> a" }, M "[a] -> a")
   end)
   it("should return list with constructors", function()
      -- assert.same({}, M"[Pattern a] -> Pattern a")
   end)
end)

describe("nested def", function()
   it("should return def", function()
      -- assert.same({
      --    T = { { "a", type = "Table" }, ret = { "a" } },
      -- }, M"(a -> a) -> a")
   end)
end)

describe("def name", function()
   it("should return def", function()
      assert.same({ { "a" }, ret = { "a" }, name = "id", source = "id :: a -> a" }, M "id :: a -> a")
   end)
end)
