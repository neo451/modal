socket = require "socket"
moonscript = require "moonscript.base"
import dump from "xi.utils"
for k, v in pairs(require "xi")
  _G[k] = v

clock\start!
host = host or "*"
port = port or 8080
s = assert(socket.bind(host, port))
i, p = s\getsockname!
assert(i, p)
print("Waiting connection from repl on " .. i .. ":" .. p .. "...")
c = assert(s\accept!)
c\settimeout(0.001)
print("Connected")

getstring = (a) ->
  -- TODO: switch statement to handle different types of messages
  if a
    func, err = moonscript.loadstring(a)
    if func
      ok, f = pcall(func)
      if ok
        -- print type f
        -- print f.querySpan(0,2)
        f
      else
        print("Execution error:", f)
    else
      print("Compilation error:", err)

listen = (client) ->
  l, e = client\receive()
  while not e do
    getstring(l)
    l, e = client\receive()

while coroutine.resume(clock.notifyCoroutine)
  listen(c)
