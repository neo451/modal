local pattern = require "modal.pattern"
local ut = require "modal.utils"
local factory = require "modal.factory"
local ui = require "modal.ui"
local params = require "modal.params"
local mt = pattern.mt
local types = require "modal.types"

local modal = {
   version = "modal dev-1",
   url = "https://github.com/noearc/modal",
}

local pairs = pairs

for name, func in pairs(factory) do
   modal[name] = func
   mt[name] = ut.method_wrap(func)
end

for name, func in pairs(types) do
   modal[name] = func
end

-- todo:
-- for _, func in pairs(ui) do
--    pattern.register(func)
-- end

for name, func in pairs(pattern) do
   modal[name] = func
end

for name, func in pairs(params) do
   modal[name] = func
   mt[name] = function(self, ...)
      return self .. func(...)
   end
end

setmetatable(modal, {
   __index = _G,
})

-- pattern.sl = ut.string_lambda(modal)
-- modal.sl = pattern.sl

--- TODO:lib pats and funcs?
modal.fonf = "bd [bd, sd] bd [bd, sd]"

-- pattern.mini = maxi(modal, false)
-- modal.mini = pattern.mini

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

return modal
