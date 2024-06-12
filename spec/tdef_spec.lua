local describe = require("busted").describe
local M = require "modal.typedef"
local it = require("busted").it
local assert = require("busted").assert

describe("id", function()
   it("should return elem", function()
      assert.same({
         T = { { "a" }, ret = { "a" } },
      }, M:new "a -> a")
   end)
   it("should return elem", function()
      assert.same({
         T = { { "Time" }, ret = { constructor = "Pattern", "a" } },
      }, M:new "Time -> Pattern a")
   end)
end)

describe("table", function()
   it("should return elem with table tag", function()
      assert.same({
         T = { { "a", type = "Table" }, ret = { "a" } },
      }, M:new "[a] -> a")
   end)
   it("should return list with constructors", function()
      -- assert.same({}, M:new "[Pattern a] -> Pattern a")
   end)
end)

describe("nested def", function()
   it("should return def", function()
      -- assert.same({
      --    T = { { "a", type = "Table" }, ret = { "a" } },
      -- }, M:new "(a -> a) -> a")
   end)
end)
