import fastcat, pure, fast, Pattern from require "xi.pattern"
require "moon.all"
C = {}

controls = { "sound", "note" }

create = (name) ->
  withVal = (v) -> { [name]: v }
  func = (args) -> fastcat(args)\fmap(withVal)
  C[name] = func

for name in *controls
  create name

-- print C.sound("a")\combineRight(C.note("b"))
--
-- print fast(2, C.sound("a"))\combineLeft(C.sound("b"))
-- print fast(2, C.sound("a"))\combineRight(C.sound("b"))
--
-- print fast(2, C.sound("a"))\combineLeft(fast(4, C.sound("b")))
-- print fast(2, C.sound("a"))\combineRight(fast(4, C.sound("b")))

return C
