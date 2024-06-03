local Stream
Stream = require("modal.stream").Stream
local Pattern
Pattern = require("modal.pattern").Pattern
local sound
sound = require("modal.params").sound
local losc = require('losc')
local abletonlink = require("abletonlink")
local busted = require("busted")
local mock = busted.mock
return describe("Stream", function()
  return describe("new", function()
    return it("should construct with SuperDirt target", function()
      local stream = Stream()
      assert.are.equal(stream.isPlaying, false)
      assert.are.equal(stream.latency, 0.3)
      return assert.is_nil(stream.pattern)
    end)
  end)
end)
