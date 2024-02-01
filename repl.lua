local socket = require("socket")
print("xi repl")
local host = "localhost"
local port = 8080
local c = assert(socket.connect(host, port))
io.write("> ")
local l = io.read()
while l and l ~= "" and not e do
  assert(c:send(l .. "\n"))
  io.write("> ")
  l = io.read()
end
