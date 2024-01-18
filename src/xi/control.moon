import fastcat from require "xi.pattern"
require "moon.all"
C = {}
C.sound = (args) -> bind_methods fastcat(args)\withValue((v) -> { sound: v })

C.note = (args) -> bind_methods fastcat(args)\withValue((v) -> { note: v })

return C
