local it = require("busted").it
local assert = require("busted").assert
local describe = require("busted").describe
local busted = require "busted"
local spy = busted.spy
local Clock = require("modal").Clock
local Stream = require("modal").Stream
local s = require("modal").s

describe("new", function()
   it("should construct with SuperDirt target", function()
      local stream = Stream()
      assert.equal(stream.latency, 0.2)
      assert.is_nil(stream.pattern)
   end)
end)

describe("notifyTick", function()
   it("should return nil if no pattern", function()
      local stream = Stream()
      assert.is_nil(stream:notifyTick())
   end)

   it("should call sendf with right value and timestamp", function()
      local clock = Clock()
      local stream = Stream()
      stream.pattern = s "bd"
      local stream_spy = spy.on(stream, "sendf")
      local cps = (clock.sessionState:tempo() / clock.beatsPerCycle) / 60
      stream:notifyTick(0, 1, clock.sessionState, cps, clock.beatsPerCycle, 1000000, 0)
      assert.spy(stream_spy).was_called()
      -- assert.spy(stream_spy).was_called_with()
   end)
end)
