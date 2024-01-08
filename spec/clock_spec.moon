require "xi.clock"
require "xi.pattern"
require "xi.stream"

describe "LinkClock", ->
  describe "construction", ->
    it "should create with defaults", ->
      clock = Clock!
      assert.are.equal clock.bpm, 120
      assert.are.equal clock.sampleRate, 1 / 20
      assert.are.equal clock.beatsPerCycle, 4
      assert.are.equal clock.running, false
      assert.are.same clock.subscribers, {}
      assert.is_not_nil clock.link
      assert.is_not_nil clock.sessionState
      assert.is_nil clock.notifyCoroutine

    it "should have a function declaring its type", ->
      clock = Clock!
      assert.are.equal "clock", clock\type!

  describe "subscribe/unsubscribe", ->
    it "should add/remove to list of subscribers", ->
      clock = Clock 120
      mySub = Stream!
      mySub.pattern = pure "i am the first"
      clock\subscribe mySub
      assert.are.equal 1, #clock.subscribers
      assert.are.equal mySub, clock.subscribers[1]
      mySub2 = Stream!
      mySub2.pattern = pure "I am the second"
      clock\subscribe mySub2
      assert.are.equal 2, #clock.subscribers
      assert.are.equal mySub, clock.subscribers[1]
      assert.are.equal mySub2, clock.subscribers[2]
      clock\unsubscribe mySub
      assert.are.equal 1, #clock.subscribers
      assert.are.equal mySub2, clock.subscribers[1]


  -- describe("notify", ->
  --     it("should call stream's notify method on tick", ->
  --         local clock = LinkClock:new()
  --         clock._link = mock(clock._link, true)
  --         clock._linkSessionState = mock(clock._linkSessionState)
  --         local stream = mock(Stream:new(), true)
  --         clock:subscribe(stream)
  --         clock:start()
  --     )
  -- )


