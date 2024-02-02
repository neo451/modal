-- Based on lua.lua - Lua 5.1 interpreter (lua.c) reimplemented in Lua.
--
-- WARNING: This is not completed but was quickly done just an experiment.
-- Fix omissions/bugs and test if you want to use this in production.
-- Particularly pay attention to error handling.
--
-- (c) David Manura, 2008-08
-- Licensed under the same terms as Lua itself.
-- Based on lua.c from Lua 5.1.3.
-- Improvements by Shmuel Zeigerman.

-- Variables analogous to those in luaconf.h
LUA_INIT = "LUA_INIT"
LUA_PROGNAME = "lua"
prompt1 = "> "
prompt2 = ">> "

LUA_QL = (x) -> "'" .. x .. "'"
lua51 = _VERSION\match("5%.1$")
-- Variables analogous to those in lua.h
LUA_RELEASE, LUA_COPYRIGHT, eof_ender
if lua51
	LUA_RELEASE = "Lua 5.1.4"
	LUA_COPYRIGHT = "Copyright (C) 1994-2008 Lua.org, PUC-Rio"
	eof_ender = LUA_QL("<eof>")
else
	LUA_RELEASE = "Lua 5.2.0"
	LUA_COPYRIGHT = "Copyright (C) 1994-2011 Lua.org, PUC-Rio"
	eof_ender = "<eof>"

EXTRA_COPYRIGHT = "lua.lua (c) David Manura, 2008-08"

MORE_COPYRIGHT = "xi repl (c) Zizhou Teng"

_G = _G
assert = assert
collectgarbage = collectgarbage
loadfile = loadfile
loadstring = loadstring or load
pcall = pcall
rawget = rawget
select = select
tostring = tostring
type = type
unpack = unpack or table.unpack
xpcall = xpcall
io_stderr = io.stderr
io_stdout = io.stdout
io_stdin = io.stdin
string_format = string.format
string_sub = string.sub
os_getenv = os.getenv
os_exit = os.exit

our_tostring = tostring

-- HACK: ?
our_print = (...) ->
	args = tuple(...)
	for i = 1, args.n
		io.write(our_tostring(args[i]), "\t")
	_G._ = args[1]
	io.write("\n")

progname = LUA_PROGNAME

-- Use external functions, if available
lua_stdin_is_tty = -> true

setsignal = ->

-- print_usage = () ->
-- 	io_stderr\write(
-- 		string_format(
-- 			"usage: %s [options] [script [args]].\n"
-- 				.. "Available options are:\n"
-- 				.. "  -e stat  execute string "
-- 				.. LUA_QL("stat")
-- 				.. "\n"
-- 				.. "  -l name  require library "
-- 				.. LUA_QL("name")
-- 				.. "\n"
-- 				.. "  -i       enter interactive mode after executing "
-- 				.. LUA_QL("script")
-- 				.. "\n"
-- 				.. "  -v       show version information\n"
-- 				.. "  --       stop handling options\n"
-- 				.. "  -        execute stdin and stop handling options\n",
-- 			progname
-- 		)
-- 	)
-- 	io_stderr\flush!

tuple = table.pack or (...) -> { n: select("#", ...), ... }

using_lsh, lsh = nil, nil

saveline = (s) ->
	if using_lsh then lsh.saveline(s)

getline = (prmt) ->
	if using_lsh then
		return lsh.readline(prmt)
	else
		io_stdout\write(prmt)
		io_stdout\flush()
		return io_stdin\read("*l")

l_message = (pname, msg) ->
	if pname then
		io_stderr\write(string_format("%s: ", pname))
	io_stderr\write(string_format("%s\n", msg))
	io_stderr\flush()

socket = require("socket")
host = "localhost"
port = 8080

c = assert(socket.connect(host, port))

-- HACK: to verbose?
get_prompt = (firstline) ->
  -- pmt = rawget(_G, firstline and "_PROMPT" or "_PROMPT2")
  pmt = firstline and prompt1 or prompt2
  tp = type pmpt
  if tp == "string" or tp == "number"
    return tostring pmt
  return firstline and prompt1 or prompt2


fetchline = (firstline) ->
  getline get_prompt firstline

imcomplete = (msg) ->
  if msg
    if string_sub(msg, -#eof_ender) == eof_ender
      return true
  return false

pushline = (firstline) ->
  b, fine = true, nil
  while fine
    b = fetchline(firstline)
    if not b
      return
    if using_lsh
      fine = lsh.checkline(b)

  if firstline and string_sub(b, 1, 1) == "="
    return "return " .. string_sub(b, 2) -- change '=' to `return'
  else
    return b

loadline = ->
	b = pushline(true)
	if not b then return -1
	 -- no input
	f, msg
	while true -- repeat until gets a complete line
		-- f, msg = loadstring(b, "=stdin")
		if not incomplete(msg) then break
		 -- cannot try to add lines?
		b2 = pushline(false)
		if not b2 then return -1
		
		b = b .. "\n" .. b2 -- join them
	

	saveline(b)
	c:send(b .. "\n")

	return f, msg


dotty = ->
  oldprogname = progname
  progname = nil
  using_lsh, lsh = pcall(require, "luaish")
  if using_lsh then
    our_tostring = lsh.tostring
  else
    print "no luaish"

  while true
    result = nil
    status, msg = loadline!
    if status == -1 then break

    if status
      result = tuple(docall(status))
      status, msg = result[1], result[2]

    report status, msg

    if status and result.n > 1
      status, msg = pcall our_print, unpack(result, 2, result.n)
      if not status
        l_message(progname, string_format("error calling %s (%s)", LUA_QL("print"), msg))

  io_stdout\write("\n")
  io_stdout\flush!
  progname = oldprogname

dotty!

