local describe = require("busted").describe
local it = require("busted").it
local assert = require("busted").assert
local gen = require "modal.gendef"

describe("gen", function()
   it("shold gen luacat definition", function()
      assert.same(
         "---@param factor Time\n---@param pat any\n---@return Pattern\nfast = function(factor, pat) end\n",
         gen "fast"
      )
   end)
end)