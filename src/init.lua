local pattern = require "pattern"
local notation = require "notation"
local ut = require "utils"
local factory = require "factory"
local theory = require "theory"
local control = require "control"
local mt = pattern.mt
local types = require "types"
local Clock = require "clock"
require "modal.ui"

local modal = {}
modal.version = "modal dev-1"
modal.url = "https://github.com/noearc/modal"

local pairs = pairs

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

setmetatable(modal, {
   __index = _G,
})

setmetatable(modal, {
   __call = function(t, override)
      for k, v in pairs(t) do
         if _G[k] ~= nil then
            local msg = "function " .. k .. " already exists in global scope."
            print("WARNING: " .. msg)
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
require("modedebug").start()

return modal
