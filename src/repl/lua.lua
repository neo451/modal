local LUA_INIT = "LUA_INIT"
local LUA_PROGNAME = "lua"
local prompt1 = "> "
local prompt2 = ">> "
local LUA_QL
LUA_QL = function(x)
  return "'" .. x .. "'"
end
local lua51 = _VERSION:match("5%.1$")
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
local socket = require("socket")
local host = "localhost"
local port = 8080
local c = assert(socket.connect(host, port))
local get_prompt
get_prompt = function(firstline)
  local pmt = firstline and prompt1 or prompt2
  local tp = type(pmpt)
  if tp == "string" or tp == "number" then
    return tostring(pmt)
  end
  return firstline and prompt1 or prompt2
end
local fetchline
fetchline = function(firstline)
  return getline(get_prompt(firstline))
end
local imcomplete
imcomplete = function(msg)
  if msg then
    if string_sub(msg, -#eof_ender) == eof_ender then
      return true
    end
  end
  return false
end
local pushline
pushline = function(firstline)
  local b, fine = true, nil
  while fine do
    b = fetchline(firstline)
    if not b then
      return 
    end
    if using_lsh then
      fine = lsh.checkline(b)
    end
  end
  if firstline and string_sub(b, 1, 1) == "=" then
    return "return " .. string_sub(b, 2)
  else
    return b
  end
end
local loadline
loadline = function()
  local b = pushline(true)
  if not b then
    return -1
  end
  _ = f, msg
  while true do
    if not incomplete(msg) then
      break
    end
    local b2 = pushline(false)
    if not b2 then
      return -1
    end
    b = b .. "\n" .. b2
  end
  saveline(b)
  _ = {
    c = send(b .. "\n")
  }
  return f, msg
end
local dotty
dotty = function()
  local oldprogname = progname
  progname = nil
  using_lsh, lsh = pcall(require, "luaish")
  if using_lsh then
    our_tostring = lsh.tostring
  else
    print("no luaish")
  end
  while true do
    local result = nil
    local status, msg = loadline()
    if status == -1 then
      break
    end
    if status then
      result = tuple(docall(status))
      status, msg = result[1], result[2]
    end
    report(status, msg)
    if status and result.n > 1 then
      status, msg = pcall(our_print, unpack(result, 2, result.n))
      if not status then
        l_message(progname, string_format("error calling %s (%s)", LUA_QL("print"), msg))
      end
    end
  end
  io_stdout:write("\n")
  io_stdout:flush()
  progname = oldprogname
end
return dotty()
