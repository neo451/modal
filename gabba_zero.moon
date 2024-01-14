x = require "xi.init"

print x._VERSION

pat = x.p(1, x.s("gabba"))
-- \fast(x.cat(1, 2, 3, 4))

clock = x.clock
clock\start!

while coroutine.resume(clock.notifyCoroutine)
	--[[ poll for user input]]
	--
	print("boop")
print("all done")
