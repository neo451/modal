socket = require "socket"
uv = require "luv"

stdin = uv.new_pipe()
stdout = uv.new_pipe()
stderr = uv.new_pipe()

stdin1 = uv.new_pipe()
stdout1 = uv.new_pipe()
stderr1 = uv.new_pipe()

handle, pid = uv.spawn("moon", {
  stdio: {stdin, stdout, stderr}
  args: {"boot.moon"}
},((code, signal) ->-- on exit
  print("exit code", code)
  print("exit signal", signal))
)


uv.read_start(stdout,(err, data) ->
  assert(not err, err)
  if data
    print("stdout chunk", stdout, data)
  else
    print("stdout end", stdout)
)

uv.read_start(stderr, (err, data) ->
  assert(not err, err)
  if data then
    print("stderr chunk", stderr, data)
  else
    print("stderr end", stderr)
)

-- repl, pid2 = uv.spawn("moon", {
--   stdio: {stdin1, stdout1, stderr1}
--   args: {"repl.moon"}
--   },((code, signal) ->-- on exit
--   print("exit code", code)
--   print("exit signal", signal))
-- )
--
print("process opened", handle, pid)
-- print("process opened", repl, pid2)
-- uv.write(stdin, "Hello World")

-- uv.shutdown(stdin,->
--   print("stdin shutdown", stdin)
--   uv.close(handle,->
--     print("process closed", handle, pid)
--   )
-- )
uv.run!
