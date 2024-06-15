local Stream = require "modal.stream"
local Pattern = require("modal.pattern").Pattern
local sound = require("modal.params").sound

local losc = require "losc"
local abletonlink = require "abletonlink"
local busted = require "busted"
local mock = busted.mock

describe("Stream", function()
   describe("new", function()
      it("should construct with SuperDirt target", function()
         local stream = Stream()
         assert.equal(stream.isPlaying, false)
         assert.equal(stream.latency, 0.3)
         assert.is_nil(stream.pattern)
      end)
   end)
end)
