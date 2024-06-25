local UI = require "modal.ui"
local P = require "modal.params"
require "modal"()

local describe = require("busted").describe
local it = require("busted").it
local assert = require("busted").assert

assert.pat = function(a, b)
   assert.same(a:show(), b:show())
end
describe("note", function()
   it("shoudl parse chords", function()
      assert.pat(stack { { note = 0 }, { note = 4 }, { note = 7 } }, P.note "c'maj")
   end)
end)
