local socket = require("socket")
local link = require("abletonlink")
local sleep
sleep = function(sec)
  return socket.sleep(sec)
end
local Clock
do
  local _class_0
  local _base_0 = {
    type = function()
      return "clock"
    end,
    start = function(self)
      if not self.running then
        self.running = true
        return self:createNotifyCoroutine()
      end
    end,
    stop = function(self)
      self.running = false
      return print("clock stopped")
    end,
    subscribe = function(self, subscriber)
      return table.insert(self.subscribers, subscriber)
    end,
    unsubscribe = function(self, subscriber)
      local position = nil
      for i, sub in ipairs(self.subscribers) do
        if sub == subscriber then
          position = i
        end
      end
      if position ~= nil then
        return table.remove(self.subscribers, position)
      end
    end,
    createNotifyCoroutine = function(self)
      self.notifyCoroutine = coroutine.create(function()
        self.link:enable(true)
        self.link:enable_start_stop_sync(true)
        local start = self.link:clock_micros()
        local ticks = 0
        local mill = 1000000
        local frame = self.sampleRate * mill
        while self.running do
          ticks = ticks + 1
          print(ticks)
          local logicalNow = math.floor(start + (ticks * frame))
          local logicalNext = math.floor(start + ((ticks + 1) * frame))
          local now = self.link:clock_micros()
          local wait = (logicalNow - now) / mill
          if wait > 0 then
            sleep(wait)
          end
          if not self.running then
            break
          end
          self.link:capture_audio_session_state(self.sessionState)
          local cps = (self.sessionState:tempo() / self.beatsPerCycle) / 60
          local cycleFrom = self.sessionState:beat_at_time(logicalNow, 0) / self.beatsPerCycle
          local cycleTo = self.sessionState:beat_at_time(logicalNext, 0) / self.beatsPerCycle
          local _list_0 = self.subscribers
          for _index_0 = 1, #_list_0 do
            local sub = _list_0[_index_0]
            sub:notifyTick(cycleFrom, cycleTo, self.sessionState, cps, self.beatsPerCycle, mill, now)
          end
          coroutine.yield()
        end
        self.linkEnabled = false
      end)
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, bpm, sampleRate, beatsPerCycle)
      if bpm == nil then
        bpm = 120
      end
      if sampleRate == nil then
        sampleRate = 1 / 20
      end
      if beatsPerCycle == nil then
        beatsPerCycle = 4
      end
      self.bpm, self.sampleRate, self.beatsPerCycle = bpm, sampleRate, beatsPerCycle
      self.link = link.create(bpm)
      self.sessionState = link.create_session_state()
      self.subscribers = { }
      self.running = false
      self.notifyCoroutine = nil
      self.latency = 0.2
    end,
    __base = _base_0,
    __name = "Clock"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  Clock = _class_0
end
return {
  Clock = Clock
}
