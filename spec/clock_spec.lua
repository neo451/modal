local Clock
Clock = require("modal.clock").Clock
local Stream
Stream = require("modal.stream").Stream
local Event
Event = require("modal.types").Event
local pure
pure = require("modal.pattern").pure
return describe("Clock", function()
   describe("construction", function()
      return it("should create with defaults", function()
         local clock = Clock()
         assert.are.equal(clock.bpm, 120)
         assert.are.equal(clock.sampleRate, 1 / 20)
         assert.are.equal(clock.beatsPerCycle, 4)
         assert.are.equal(clock.running, false)
         assert.are.same(clock.subscribers, {})
         assert.is_not_nil(clock.link)
         assert.is_not_nil(clock.sessionState)
         return assert.is_nil(clock.notifyCoroutine)
      end)
   end)
   return describe("subscribe/unsubscribe", function()
      return it("should add/remove to list of subscribers", function()
         local clock = Clock(120)
         local mySub = Stream()
         mySub.pattern = pure "i am the first"
         clock:subscribe(mySub)
         assert.are.equal(1, #clock.subscribers)
         assert.are.equal(mySub, clock.subscribers[1])
         local mySub2 = Stream()
         mySub2.pattern = pure "I am the second"
         clock:subscribe(mySub2)
         assert.are.equal(2, #clock.subscribers)
         assert.are.equal(mySub, clock.subscribers[1])
         assert.are.equal(mySub2, clock.subscribers[2])
         clock:unsubscribe(mySub)
         assert.are.equal(1, #clock.subscribers)
         return assert.are.equal(mySub2, clock.subscribers[1])
      end)
   end)
end)
