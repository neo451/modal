local describe = require("busted").describe
local it = require("busted").it
local assert = require("busted").assert

require "modal"()

assert.pat = function(a, b)
   assert.same(a(0, 1), b(0, 1))
end

describe("table arith", function()
   it("", function()
      -- assert.pat(reify { pan = 0.2 }, reify { pan = 0.1 } + 0.1)
      -- assert.pat(reify { pan = 0.2 }, reify { pan = 0.1 } + pure(0.1))
      -- assert.pat(reify { note = 2, pan = 0.2 }, reify { note = 1, pan = 0.1 } + { note = 1, pan = 0.1 })
      -- assert.pat(
      --    reify { s = "bd", note = 2, pan = 0.2 },
      --    reify { s = "bd", note = 1, pan = 0.1 } + { note = 1, pan = 0.1 }
      -- )
      assert.same(ValueMap { pan = 0.2 }, ValueMap { pan = 0.1 } + ValueMap { pan = 0.1 })
   end)
   it("does chain", function()
      assert.same(ValueMap { pan = 3 }, ValueMap { pan = 1 } + ValueMap { pan = 1 } + ValueMap { pan = 1 })
   end)
end)
