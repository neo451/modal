socket = require "socket"
print "xi repl"
host = "localhost"
port = 8080
c = assert(socket.connect(host, port))
io.write "> "
l = io.read()
while l and l ~= ""
  assert(c\send(l .. "\n"))
  io.write("> ")
  l = io.read()
