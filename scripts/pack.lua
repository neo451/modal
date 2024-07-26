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
      if f:find "%.lua$" then
         hndl = f:gsub("%.lua$", ""):gsub("^[/\\]", ""):gsub("/", "."):gsub("\\", ".")
         files[hndl] = io.open(root .. f)
      end
   end
end

scandir "src/"

local modules = { "ut", "types", "a2s", "notation", "theory", "clock", "factory", "control", "pattern", "losc" }

local function not_modname(line)
   for _, name in ipairs(modules) do
      if line:match(('local %s = require "%s"'):format(name, name)) then
         return false
      end
   end
   return true
end

local function get_content(name, file, no_req)
   no_req = no_req or true
   local contents = {}
   for line in file:lines() do
      if no_req then
         if not_modname(line) and not line:match(("local %s = {}"):format(name)) then
            contents[#contents + 1] = "   " .. line
         end
      else
         contents[#contents + 1] = "   " .. line
      end
   end
   contents[#contents] = nil
   local str = table.concat(contents, "\n")
   return str
end

local function wrap(name, file, no_req)
   local format = [[do
%s
end
]]
   return format:format(get_content(name, file, no_req))
end

local requires = [==[
--[[lit-meta
  name = "noearc/modal"
  version = "0.0.1.1"
  homepage = "https://github.com/noearc/modal"
  description = "tidal cycles in lua!"
  license = "GPL3"
]]
local ut = {}
local pattern = {}
local control = {}
local types = {}
local theory = {}
local notation = {}
local a2s = {}
local factory = {}
local losc = {}
]==]

-- load("lulpeg", false)
local header = files["lulpeg"]:read "*a" .. "\n" .. requires
-- .. files["losc"]:read "*a" .. requires

function load(name, no_req)
   header = header .. "\n" .. wrap(name, files[name], no_req)
end
load "losc"
load "ut"
load "types"
load "a2s"
load "notation"
load "theory"
load "clock"
load "factory"
load "control"
load "pattern"
header = header .. "\n" .. get_content("init", files["init"])
load "repl"
load "server"
header = header .. "\n" .. "modal.ut = ut"
header = header .. "\n" .. "return modal"

print(header)

-- TODO: lfs
-- TODO: stylua the result if avaliable
