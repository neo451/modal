socket = require "socket"
-- moonscript = require "moonscript.base"
yue = require "yue"
-- import dump from "xi.moondump"
-- require "moon.all"
for k, v in pairs(require "xi")
  _G[k] = v

-- clock = DefaultClock
-- clock\start!
-- host = "*"
-- port = 8080
-- s = socket.bind(host, port)
-- i, p = s\getsockname!
-- -- assert(i, p)
-- c = s\accept!
-- c\settimeout(0.00000000000001)
-- print("Connected to Xi")

getstring = (a) ->
  if a
    func, err = loadstring(a)
    if func
      ok, res = pcall(func)
      if ok
        print res
      else
        print("Execution error:", ok)
    else
      print("Compilation error:", err)

listen = (client) ->
  l, e = client\receive()
  while not e do
    l, e = client\receive()
    getstring(l)

codes, err, globals = yue.loadfile("d1.yue")
codes1, err, globals = yue.read_file("d1.yue")
a = yue.to_lua codes1
print type a
func = loadstring a
print func!
-- while coroutine.resume(clock.notifyCoroutine)
--   listen(c)
