local pattern = require "pattern"
local notation = require "notation"
local factory = require "factory"
local theory = require "theory"
local ut = require "ut"
local control = require "control"
local mt = pattern.mt
local types = require "types"
-- local Clock = require "clock"
-- require "modal.ui"

local modal = {}
modal.version = "modal dev-1"
modal.url = "https://github.com/noearc/modal"

local pairs = pairs

-- FIXME: later use this, not directly in global scope if not imported
modal.Clock = Clock

for name, func in pairs(notation) do
   modal[name] = func
end

for name, func in pairs(theory) do
   modal[name] = func
end

for name, func in pairs(factory) do
   modal[name] = func
   mt[name] = ut.method_wrap(func)
end

for name, func in pairs(types) do
   modal[name] = func
end

for name, func in pairs(pattern) do
   modal[name] = func
end

modal.notation = notation

-- modal.maxi = notation.maxi(modal)

setmetatable(modal, {
   __index = _G,
})

setmetatable(modal, {
   __call = function(t, verb, override)
      for k, v in pairs(t) do
         if _G[k] ~= nil then
            local msg = "function " .. k .. " already exists in global scope."
            if verb then
               print("WARNING: " .. msg)
            end
            if override then
               _G[k] = v
               print("WARNING: " .. msg .. " Overwritten.")
            end
         else
            _G[k] = v
         end
      end
   end,
})
-- require("modedebug").start()

return modal
