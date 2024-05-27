local M = require("modal")
local op = require("modal.pattern").op
local describe = require("busted").describe
local it = require("busted").it
local assert = require("busted").assert

describe("In", function()
   it("should take structure from left", function()
      -- local res = op.In.add("1 2", "2 1")
      -- assert.same(M.fast(2, 3), res)
      local res2 = op.In.add("1 2", "4 5 6")
      assert.same(M.fast(2, 3), res2)
      -- print(op.Out.add("1 2", "4 5 6"))
   end)
end)

describe("Out", function()
   it("should take structure from letf", function()
      local res2 = op.Out.add("1 2", "4 5 6")
      assert.same(M.fast(2, 3), res2)
   end)
end)
--
-- describe("Mix", function()
--    it("should take structure from letf", function()
--       assert.same(M.fast(2, 3), op.In.add("1 2", "2 1"))
--    end)
-- end)
--
-- describe("Squeeze", function()
--    it("should take structure from letf", function()
--       assert.same(M.fast(2, 3), op.In.add("1 2", "2 1"))
--    end)
-- end)
