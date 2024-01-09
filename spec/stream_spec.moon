require "xi.stream"
require "xi.pattern"
require "xi.control"
losc = require('losc')
abletonlink = require("abletonlink")
busted = require "busted"
mock = busted.mock

describe "Stream", ->
  describe "new", ->
    it "should construct with SuperDirt target", ->
      stream = Stream!
      assert.are.same stream.target, StreamTarget
      assert.are.equal stream.isPlaying, false
      assert.are.equal stream.latency, 0.2
      assert.is_nil stream.pattern


    it "should have a function declaring its type", ->
      stream = Stream!
      assert.are.equal "stream", stream\type!

  -- describe "notifyTick", ->
  --   it "should s osc message when called", ->
  --     stream = Stream!
  --     stream.osc = mock(losc.new, true)
  --     stream.osc.new_message = busted.spy()
  --     stream.osc.s = busted.spy()
  --     stream.pattern = sound "bd"
  --     -- stream.pattern = pure "bd"
  --     session_state = abletonlink.create_session_state()
  --     stream\notifyTick(0, 1, session_state, 0.5, 4, 10000, 333)
  --
  --     assert.spy(stream.osc.new_message).was_called()
  --     assert.spy(stream.osc.s).was_called()



