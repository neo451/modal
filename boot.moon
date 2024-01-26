socket = require "socket"
yue = require "yue"
for k, v in pairs(require "xi")
  _G[k] = v

clock = DefaultClock
clock\start!
host = "*"
port = 8080
s = socket.bind(host, port)
i, p = s\getsockname!
-- assert(i, p)
c = s\accept!
c\settimeout(0.00000000000001)
print("Connected to Xi")

getstring = (a) ->
  if a
    func, err = yue.loadstring(a)
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
  getstring(l)
  while not e do
    l, e = client\receive()
    getstring(l)

while coroutine.resume(clock.notifyCoroutine)
  listen(c)
