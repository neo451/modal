package = "xi"
version = "dev-1"
source = {
	url = "git+ssh://git@github.com/noearc/xi.git",
}
description = {
	summary = "a port of the tidalcycles pattern language to lua",
	detailed = [[
xi is an experimental port of the livecoding music language [Tidalcycles](http://tidalcycles.org/) to the lua programming language.
This project follows the footsteps of [vortex](https://github.com/tidalcycles/vortex) and [strudel](https://strudel.tidalcycles.org).]],
	homepage = "https://github.com/noearc/xi",
	license = "GPL3",
}
dependencies = {
	"lua >= 5.1",
	"luasec >= 1.2.0-1",
	"losc >= 1.0.1-1",
	"luasocket >= 3.1.0-1",
	"abletonlink >= 1.0.0-1",
	"lpeg >= 1.1.0-1",
	"ldoc >= 1.0.1-1",
	"moonscript >= 0.5.0-1",
}
build = {
	type = "builtin",
	modules = {
		-- ["xi.init"] = "src/xi/init.lua",
		["xi.utils"] = "src/xi/utils.lua",
		["xi.dump"] = "src/xi/dump.lua",
		["xi.fraction"] = "src/xi/fraction.lua",
		["xi.arc"] = "src/xi/arc.lua",
		["xi.event"] = "src/xi/event.lua",
		["xi.state"] = "src/xi/state.lua",
		["xi.stream"] = "src/xi/stream.lua",
		-- ["xi.pattern"] = "src/xi/pattern.lua",
		-- ["xi.control"] = "src/xi/control.lua",
		-- ["xi.link_clock"] = "src/xi/link_clock.lua",
		-- ["xi.pattern_factory"] = "src/xi/pattern_factory.lua",
		-- ["xi.mini"] = "src/xi/mini/init.lua",
		-- ["xi.mini.grammar"] = "src/xi/mini/grammar.lua",
		-- ["xi.mini.visitor"] = "src/xi/mini/visitor.lua",
		-- ["xi.mini.interpreter"] = "src/xi/mini/interpreter.lua",
	},
	-- install = {
	--    bin = {"bin/xi"}
	-- }
}
