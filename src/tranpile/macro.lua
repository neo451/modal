require("moon.all")
pp = require("metalua.pprint").print
local lpeg = require("lpeg")

local V, R, P, S = lpeg.V, lpeg.R, lpeg.P, lpeg.S
local C, Ct = lpeg.C, lpeg.Ct

local root = V("root")
local symb = V("symb")
local call = V("call")
local def = V("def")
local num = V("num")
local atom = V("atom")
local dollar = V("dollar")
local paren = V("paren")
local op = V("op")
local binop = V("binop")
local item = V("item")
local mini = V("mini")
local pr = V("pr")
local pm = V("pm")
local spm = V("spm")
local mop = V("mop")

local function Id(a)
	return { tag = "Id", a }
end

local function Reify(a)
	return { tag = "Call", Id("reify"), a }
end

local function ppr(...)
	local args = { ... }
	local acc = {}
	for _, v in pairs(args) do
		v.tag = "String"
		acc[#acc + 1] = v
	end
	return { tag = "Call", Id("fastcat"), unpack(acc) }
end

local function pspm(...)
	local args = { ... }
	local acc = {}
	for _, v in pairs(args) do
		-- TODO: maybe no need for distinction
		if v.tag == "Id" then
			v.tag = "String"
		end
		acc[#acc + 1] = v
	end
	return { tag = "Call", Id("slowcat"), unpack(acc) }
end

local function psymb(s)
	return { tag = "Id", s }
end

local function pnum(n)
	return { tag = "Number", n }
end

local opsymb = {
	["+"] = "add",
	["-"] = "sub",
	["*"] = "mul",
	["/"] = "div",
	["^"] = "pow",
	["%"] = "mod",
	-- TODO: tidal ops!!
}

local function pcal(...)
	local args = { ... }
	if opsymb[args[1][1]] then
		local opname = opsymb[args[1][1]]
		table.remove(args, 1)
		return { tag = "Op", opname, unpack(args) }
	end
	return { tag = "Call", ... }
end

local function pparen(...)
	-- p({ ... })
	-- if select(1, ...)[1].tag == "Call" then
	-- 	return select(1, ...)[1]
	-- end
	-- return { tag = "Paren", ... }
	return { ... }
end

local function pop(a, b)
	return { tag = "Op", a, b }
end

local function id(a)
	return a
end

local ws = S(" \n\r\t") ^ 0
local rules = {
	[1] = root,
	op = S("-+/&%^"),
	mop = S("[]<>{}") / id,
	root = ws * (call + binop + symb + paren + mini) * ws,
	num = R("09") ^ 1 / pnum,
	symb = (R("AZ", "az") + op) ^ 1 / psymb,
	atom = ws * (symb + num + paren + dollar + binop + mini) / id,
	item = num + symb + paren,
	binop = item * ws * op * ws * item / pop,
	call = (atom ^ 1) / pcal,
	mini = pr + spm,
	pr = P("[") * ws * ((((num + symb + mini + paren) * ws) ^ 1) / ppr) * ws * P("]"),
	spm = P("<") * ws * ((((num + symb + mini + paren) * ws) ^ 1) / pspm) * ws * P(">"),
	paren = P("(") * ws * (binop + call) * ws * P(")") / pparen,
	dollar = P("$") * ws * call / pparen,
}

local pat = Ct(C(rules))

local function read(str)
	return pat:match(str)[2]
end

-- local ast = read([[
-- 	slow 4 (fast 4 (pure 1))
-- ]])

local ast = read([[
slow 4 (fast [4 8] (pure 1))
]])

pp(ast)

local mlc = require("metalua.compiler").new()

local str = mlc:ast_to_src(ast)

print(str)

pp.print(loadstring("require'modal'(); return " .. str)()(0, 3))
