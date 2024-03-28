local pattern = require("xi.pattern")
local pattern_factory = require("xi.pattern_factory")
local C = require("xi.control")
local drawline
drawline = require("xi.drawline").drawline
local xi = {
  _VERSION = "xi dev-1",
  _URL = "https://github.com/noearc/xi",
  _DESCRIPTION = "A language for algorithmic pattern. Tidalcycles for moonscript"
}
xi.drawline = drawline
for name, func in pairs(pattern) do
  if name ~= "Pattern" then
    xi[name] = func
  end
end
for name, func in pairs(pattern_factory) do
  xi[name] = func
end
for name, func in pairs(C) do
  xi[name] = func
end
setmetatable(xi, {
  __call = function(t, override)
    for k, v in pairs(t) do
      if _G[k] ~= nil then
        local msg = 'function ' .. k .. ' already exists in global scope.'
        if override then
          _G[k] = v
          print('WARNING: ' .. msg .. ' Overwritten.')
        end
      else
        _G[k] = v
      end
    end
  end
})
return xi
