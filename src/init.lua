local pattern = require("modal.pattern")
local pattern_factory = require("modal.pattern_factory")
local P = require("modal.params")
local drawline
drawline = require("modal.drawline").drawline
local modal = {
  _VERSION = "modal dev-1",
  _URL = "https://github.com/noearc/modal",
  _DESCRIPTION = "A language for algorithmic pattern. Tidalcycles for moonscript"
}
modal.drawline = drawline
for name, func in pairs(pattern) do
  if name ~= "Pattern" then
    modal[name] = func
  end
end
for name, func in pairs(pattern_factory) do
  modal[name] = func
end
for name, func in pairs(P) do
  modal[name] = func
end
setmetatable(modal, {
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
return modal
