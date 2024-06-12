local pattern = require "modal.pattern"
local ut = require "modal.utils"
local pattern_factory = require "modal.pattern_factory"
local ui = require "modal.ui"
local P = require "modal.params"
local drawline = require "modal.drawline"
local lib = require "modal.lib"
local mt = pattern.mt
local Pattern = pattern.Pattern
local maxi = require "modal.maxi"

local modal = {
   _VERSION = "modal dev-1",
   _URL = "https://github.com/noearc/modal",
   _DESCRIPTION = "A language for algorithmic pattern. Tidalcycles for moonscript",
}

modal.drawline = drawline

for name, func in pairs(pattern_factory) do
   modal[name] = func
   mt[name] = func
end

-- for name, pat in pairs(lib) do
--    modal[name] = pattern.reify(pat)
-- end

for _, func in pairs(ui) do
   pattern.register(func)
end

for name, func in pairs(pattern) do
   if name ~= "Pattern" then
      modal[name] = func
   end
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

if jit then
   local reify = modal.reify
   getmetatable("").__add = function(a, b)
      return reify(a) + reify(b)
   end

   getmetatable("").__mul = function(a, b)
      return reify(a) * reify(b)
   end

   getmetatable("").__div = function(a, b)
      return reify(a) / reify(b)
   end

   getmetatable("").__sub = function(a, b)
      return reify(a) - reify(b)
   end

   getmetatable("").__pow = function(a, b)
      return reify(a) ^ reify(b)
   end

   getmetatable("").__mod = function(a, b)
      return reify(a) % reify(b)
   end

   getmetatable("").__concat = function(a, b)
      return reify(a) .. reify(b)
   end

   getmetatable("").__index = function(a, b)
      return reify(a)[b]
   end
end

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
