import fastcat, pure, Pattern from require "xi.pattern"
require "moon.all"
C = {}
-- C.sound = (args) -> bind_methods fastcat(args)\withValue((v) -> { sound: v })
--
-- C.note = (args) -> bind_methods fastcat(args)\withValue((v) -> { note: v })

controls = { "sound", "note" }

create = (name) ->
  withVal = (v) -> { [name]: v }
  func = (args) -> fastcat(args)\fmap(withVal)
  pfunc = (args) => @withValue(withVal)
  Pattern.__base[name] = pfunc
  C[name] = func

for name in *controls
  create name

-- print C.sound("sd")

pure("sd").sound()

-- p C.sound("a")\combineRight({C.sound("b"), C.sound("c")})
return C
