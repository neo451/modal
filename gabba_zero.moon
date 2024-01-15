x = require "xi"

print x._VERSION

x.p 1, x.s("bd sd")\fast(x.cat(1, 2, 3, 4))
-- x.p 2, x.s"cp!!"
x.p 3, x.s"hh*7?"

export clock = x.clock
clock\start!

while coroutine.resume(clock.notifyCoroutine)
  a = 1
