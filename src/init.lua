local pattern = require "modal.pattern"
local ut = require "modal.utils"
local factory = require "modal.factory"
local ui = require "modal.ui"
local P = require "modal.params"
local drawLine = require "modal.drawline"
local lib = require "modal.lib"
local mt = pattern.mt
local maxi = require "modal.maxi"
local types = require "modal.types"

local modal = {
   _VERSION = "modal dev-1",
   _URL = "https://github.com/noearc/modal",
   _DESCRIPTION = "A language for algorithmic pattern. Tidalcycles for moonscript",
}

modal.drawLine = drawLine

for name, func in pairs(factory) do
   modal[name] = func
   mt[name] = ut.method_wrap(func)
end

for name, func in pairs(types) do
   modal[name] = func
end

for name, pat in pairs(lib) do
   modal[name] = pattern.reify(pat)
end

-- TODO:
-- for _, func in pairs(ui) do
--    pattern.register(func)
-- end

for name, func in pairs(pattern) do
   modal[name] = func
end

for name, func in pairs(P) do
   modal[name] = func
   mt[name] = function(self, ...)
      return self .. func(...)
   end
end

-- TODO: update env??
pattern.sl = ut.string_lambda(modal)
modal.sl = pattern.sl

pattern.mini = maxi(modal, false)
modal.mini = pattern.mini

setmetatable(modal, {
   __call = function(t, override)
      for k, v in pairs(t) do
         if _G[k] ~= nil then
            local msg = "function " .. k .. " already exists in global scope."
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

return modal
