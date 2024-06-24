local Clock = require "modal.clock"
local DefaultClock = Clock()
local M = {}

function M.p(key, pattern)
   DefaultClock:subscribe(key, pattern)
   return pattern
end

-- TODO: cause server to freeze ...
function M._p(key)
   DefaultClock:unsubscribe(key)
end

M.p_ = M._p

function M.hush()
   for i, _ in pairs(DefaultClock.subscribers) do
      DefaultClock:unsubscribe(i)
   end
end

-- function M.panic()
--    M.hush()
--    once(s "superpanic")
-- end
-- panic :: Tidally => IO ()
-- panic = hush >> once (sound "superpanic")

for i = 1, 16 do
   if i <= 12 then
      M["d" .. i] = function(a)
         return M.p(i, a:orbit(i - 1))
      end
   else
      M["d" .. i] = function(a)
         return M.p(i, a)
      end
   end
   M["_d" .. i] = function()
      return M._p(i)
   end
   M["d" .. i .. "_"] = function()
      return M._p(i)
   end
end

M.DefaultClock = DefaultClock

function M.setcps(cps)
   DefaultClock:setcps(cps)
end

function M.setbpm(bpm)
   DefaultClock:setbpm(bpm)
end

M.bpm = M.setbpm
M.cps = M.setcps

-- | 'hush' then execute the given action.
-- only :: Tidally => IO () -> IO ()
-- only = (hush >>)

-- -- | See 'Sound.Tidal.Stream.streamReplace'.
-- p :: Tidally => ID -> ControlPattern -> IO ()
-- p = streamReplace tidal
--
-- -- | Silences a specific stream, regardless of ControlPattern input. Useful for rapid muting of streams
-- _p :: Tidally => ID -> ControlPattern -> IO ()
-- _p k _ = streamReplace tidal k silence
--
-- -- | Silences a specific stream, regardless of ControlPattern input. Useful for rapid muting of streams
-- p_ :: Tidally => ID -> ControlPattern -> IO ()
-- p_ = _p
--
-- -- | See 'Sound.Tidal.Stream.streamHush'.
-- hush :: Tidally => IO ()
-- hush = streamHush tidal
--
-- panic :: Tidally => IO ()
-- panic = hush >> once (sound "superpanic")

return M
