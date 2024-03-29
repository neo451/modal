package = "xi"
version = "dev-1"
source = {
	url = "git+ssh://git@github.com/noearc/xi.git",
}
description = {
	summary = "a port of the tidalcycles pattern language to lua",
	detailed = [[
xi is an experimental port of the livecoding music language [Tidalcycles](http://tidalcycles.org/) to the lua programming language.
This project follows the footsteps of [vortex](https://github.com/tidalcycles/vortex) and [strudel](https://strudel.tidalcycles.org) and tranquility.]],
	homepage = "https://github.com/noearc/xi",
	license = "GPL3",
}

dependencies = {
	"lua >= 5.1",
	"losc >= 1.0.1-1",
	-- TODO: dump this after clock with uv
	"luasocket >= 3.1.0-1",
	"abletonlink >= 1.0.0-1",
	"lpeg >= 1.1.0-1",
	-- "yuescript >= 0.21.3-1",
}

build = {
	type = "builtin",
	modules = {
		["xi.init"] = "src/init.lua",

		["xi.utils"] = "src/core/utils.lua",
		["xi.fraction"] = "src/core/fraction.lua",
		["xi.bitop"] = "src/core/bitop.lua",
		["xi.drawline"] = "src/core/drawline.lua",
		["xi.inspect"] = "src/core/inspect.lua",
		["xi.fun"] = "src/core/fun.lua",
		["xi.stream"] = "src/core/stream.lua",

		["xi.types"] = "src/core/types.lua",
		["xi.pattern"] = "src/core/pattern.lua",
		["xi.control"] = "src/core/control.lua",
		["xi.pattern_factory"] = "src/core/pattern_factory.lua",

		["xi.clock"] = "src/clock/clock.lua",

		["xi.euclid"] = "src/core/euclid.lua",
		["xi.scales"] = "src/core/scales.lua",
		["xi.chords"] = "src/core/chords.lua",

		["xi.mini"] = "src/core/mini.lua",

		-- ["xi.luaish"] = "src/repl/luaish.lua",
		["xi.repl"] = "src/repl/repl.lua",
	},
	install = {
		bin = {
			"bin/xi",
		},
	},
}
