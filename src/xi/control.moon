require "xi.pattern"

export sound = (args) -> fastcat(args)\withValue((v) -> { sound: v })
