local pattern = require("xi.pattern")
local pattern_factory = require("xi.pattern_factory")
local C
C = require("xi.pattern").C
local drawline
drawline = require("xi.drawline").drawline
local xi = {
  _VERSION = "xi dev-1",
  _URL = "https://github.com/noearc/xi",
  _DESCRIPTION = "A language for algorithmic pattern. Tidalcycles for moonscript"
}
xi.drawline = drawline
for name, func in pairs(pattern) do
  if i ~= "C" and i ~= "Pattern" then
    xi[name] = func
  end
end
for name, func in pairs(pattern_factory) do
  xi[name] = func
end
for name, func in pairs(C) do
  xi[name] = func
end
return xi
