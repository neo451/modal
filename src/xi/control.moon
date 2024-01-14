import fastcat from require "xi.pattern"

C = {}
C.sound = (args) -> fastcat(args)\withValue((v) -> { sound: v })

return C
