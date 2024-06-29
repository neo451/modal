local header = [[
local ut = {}
local pattern = {}
local params = {}
local types = {}
local theory = {}
local notation = {}
local a2s = {}
local factory = {}
local lpeg = require"lpeg"
local socket = require "socket"
local al = require "abletonlink"
local losc = require "losc"
_G.struct = nil
local plugin = require "losc.plugins.udp-socket"
local Clock
]]

-- path = "../src/core/"

files = {}

local fs = {
   dir = function(path)
      local listing = io.popen("ls " .. path):read "*all"
      local files = {}
      for file in listing:gmatch "[^\n]+" do
         files[file] = true
      end
      return next, files
   end,
   attributes = function()
      return {}
   end,
}

local function scandir(root)
   -- adapted from http://keplerproject.github.com/luafilesystem/examples.html
   local hndl
   for f in fs.dir(root) do
      print(f)
      if f:find "%.lua$" then
         hndl = f:gsub("%.lua$", ""):gsub("^[/\\]", ""):gsub("/", "."):gsub("\\", ".")
         files[hndl] = io.open(root .. f)
      end
   end
end

scandir "src/core/"

files["init"] = io.open "src/init.lua"
files["clock"] = io.open "src/clock/clock.lua"
files["params"] = io.open "src/params/params.lua"
-- p(files)

local function get_content(name, file)
   local contents = {}
   for i in file:lines() do
      if not i:find "require" and not i:match(("local %s = {}"):format(name)) then
         contents[#contents + 1] = i
      end
   end
   contents[#contents] = nil
   local str = table.concat(contents, "\n")
   return str
end

local function wrap(name, file)
   local format = [[
do
   %s
end
   ]]
   return format:format(get_content(name, file))
end

function load(name)
   header = header .. "\n" .. wrap(name, files[name])
end

load "ut"
load "types"
load "a2s"
load "notation"
load "theory"
load "clock"
load "factory"
load "pattern"
load "params"
header = header .. "\n" .. get_content("init", files["init"])
header = header .. "\n" .. "return modal"

local file = io.open("modal.lua", "w")

-- TODO: lfs
-- TODO: stylua the result if avaliable

if file then
   file:write(header)
end
