local LUA_INIT = "LUA_INIT"
local LUA_PROGNAME = "lua"
local LUA_PROMPT = "> "
local LUA_PROMPT2 = ">> "
local LUA_QL
LUA_QL = function(x)
  return "'" .. x .. "'"
end
local lua51 = {
  _VERSION = match("5%.1$")
}
local _ = LUA_RELEASE, LUA_COPYRIGHT, eof_ender
if lua51 then
  local LUA_RELEASE = "Lua 5.1.4"
  local LUA_COPYRIGHT = "Copyright (C) 1994-2008 Lua.org, PUC-Rio"
  local eof_ender = LUA_QL("<eof>")
else
  local LUA_RELEASE = "Lua 5.2.0"
  local LUA_COPYRIGHT = "Copyright (C) 1994-2011 Lua.org, PUC-Rio"
  local eof_ender = "<eof>"
end
local EXTRA_COPYRIGHT = "lua.lua (c) David Manura, 2008-08"
local MORE_COPYRIGHT = "xi repl (c) Zizhou Teng"
local _G = _G
local assert = assert
local collectgarbage = collectgarbage
local loadfile = loadfile
local loadstring = loadstring or load
local pcall = pcall
local rawget = rawget
local select = select
local tostring = tostring
local type = type
local unpack = unpack or table.unpack
local xpcall = xpcall
local io_stderr = io.stderr
local io_stdout = io.stdout
local io_stdin = io.stdin
local string_format = string.format
local string_sub = string.sub
local os_getenv = os.getenv
local os_exit = os.exit
local our_tostring = tostring
local our_print
our_print = function(...)
  local args = tuple(...)
  for i = 1, args.n do
    io.write(our_tostring(args[i]), "\t")
  end
  _G._ = args[1]
  return io.write("\n")
end
local progname = LUA_PROGNAME
local lua_stdin_is_tty
lua_stdin_is_tty = function()
  return true
end
local setsignal
setsignal = function() end
local tuple = table.pack or function(...)
  return {
    n = select("#", ...),
    ...
  }
end
local using_lsh, lsh = nil, nil
local saveline
saveline = function(s)
  if using_lsh then
    return lsh.saveline(s)
  end
end
local getline
getline = function(prmt)
  if using_lsh then
    return lsh.readline(prmt)
  else
    io_stdout:write(prmt)
    io_stdout:flush()
    return io_stdin:read("*l")
  end
end
local l_message
l_message = function(pname, msg)
  if pname then
    io_stderr:write(string_format("%s: ", pname))
  end
  io_stderr:write(string_format("%s\n", msg))
  return io_stderr:flush()
end
