local M = require "modal"
-- local pure = require("modal").pure
require "modal"()
local describe = require("busted").describe
local it = require("busted").it
local assert = require("busted").assert

local DefaultClock = M.DefaultClock
-- local hush = M.hush
-- local p = M.p

describe("p", function()
   it("register stream of patterns to clock", function()
      p(1, pure "helloooo")
      assert.same(pure "helloooo"(0, 1), DefaultClock.subscribers[1].pattern(0, 1))
      assert.same(pure "helloooo"(0, 1), DefaultClock.subscribers[1].pattern(0, 1))
   end)
   it("register new pattern to existing stream", function()
      p(1, pure "new hellooo")
      assert.same(pure "new hellooo"(0, 1), DefaultClock.subscribers[1].pattern(0, 1))
   end)
end)

describe("hush", function()
   it("clear all streams", function()
      hush()
      assert.same({}, DefaultClock.subscribers)
   end)
end)
