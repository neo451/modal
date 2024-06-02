tl = require "tl"
require "moon.all"
-- p(tl)

a = [[
xi = require"xi"
return xi.fastcat(1)
]]

ast = tl.parse(a, "@internal")
ok = tl.type_check(ast, { filename = "<internal>", lax = false, gen_compat = "off" })

p(ok.type_errors)
p(ok.warnings)

fn = tl.load(a)
print(fn())
