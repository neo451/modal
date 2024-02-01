local socket = require("socket")
local yue = require("yue")
for k, v in pairs(require("xi")) do
  _G[k] = v
end
local clock = DefaultClock
clock:start()
local host = "*"
local port = 8080
local s = socket.bind(host, port)
local i, p = s:getsockname()
local c = s:accept()
c:settimeout(0.00000000000001)
print("Connected to Xi")
local getstring
getstring = function(a)
  if a then
    local lua_code = yue.to_lua(a)
    local func, err = loadstring(lua_code)
    if func then
      local ok, res = pcall(func)
      if ok then
        return print(res)
      else
        return print("Execution error:", ok)
      end
    else
      return print("Compilation error:", err)
    end
  end
end
local listen
listen = function(client)
  local l, e = client:receive()
  getstring(l)
  while not e do
    l, e = client:receive()
    getstring(l)
  end
end
while coroutine.resume(clock.notifyCoroutine) do
  listen(c)
end