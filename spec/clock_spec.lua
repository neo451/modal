local Clock = require "modal.clock"
local Stream = require "modal.stream"
local pure = require("modal.pattern").pure
local describe = require("busted").describe
local it = require("busted").it
local assert = require("busted").assert

describe("construction", function()
   it("should create with defaults", function()
      local clock = Clock()
      assert.equal(clock.bpm, 120)
      assert.equal(clock.sampleRate, 1 / 20)
      assert.equal(clock.beatsPerCycle, 4)
      assert.equal(clock.running, false)
      assert.same(clock.subscribers, {})
      assert.is_not_nil(clock.link)
      assert.is_not_nil(clock.sessionState)
      assert.is_nil(clock.notifyCoroutine)
   end)
end)

describe("subscribe/unsubscribe", function()
   it("should add/remove to list of subscribers", function()
      local clock = Clock(120)
      local mySub = Stream()
      mySub.pattern = pure "i am the first"
      clock:subscribe(1, mySub)
      assert.equal(1, #clock.subscribers)
      assert.equal(mySub, clock.subscribers[1])
      local mySub2 = Stream()
      mySub2.pattern = pure "I am the second"
      clock:subscribe(2, mySub2)
      assert.equal(2, #clock.subscribers)
      assert.equal(mySub, clock.subscribers[1])
      assert.equal(mySub2, clock.subscribers[2])
      clock:unsubscribe(1)
      assert.equal(1, #clock.subscribers)
      assert.equal(mySub2, clock.subscribers[1])
   end)
end)
