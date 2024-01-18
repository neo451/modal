socket = require "socket"
print "xi repl"
host = host or "localhost"
port = port or 8080
-- print("Attempting connection to host '" .. host .. "' and port " .. port .. "...")
c = assert(socket.connect(host, port))
-- print("Connected! Please type stuff (empty line to stop):")
io.write "> "
l = io.read()
while l and l ~= "" and not e
  assert(c\send(l .. "\n"))
  io.write("> ")
  l = io.read()
