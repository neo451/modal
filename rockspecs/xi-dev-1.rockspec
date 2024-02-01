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
	"luasec >= 1.2.0-1",
	"losc >= 1.0.1-1",
	"luasocket >= 3.1.0-1",
	"abletonlink >= 1.0.0-1",
	"lpeg >= 1.1.0-1",
	"ldoc >= 1.0.1-1",
	"moonscript >= 0.5.0-1",
	"yuescript >= 0.21.3-1",
	"fun >= 0.1.3-1",
}

build = {
	type = "builtin",
	modules = {},
	install = {
		lua = {
			["xi.init"] = "xi-core/init.lua",
			["xi.utils"] = "xi-core/utils.lua",
			["xi.fraction"] = "xi-core/fraction.lua",
			["xi.drawline"] = "xi-core/drawline.lua",
			["xi.types"] = "xi-core/types.lua",
			["xi.stream"] = "xi-core/stream.lua",
			["xi.pattern"] = "xi-core/pattern.lua",
			["xi.control"] = "xi-core/control.lua",
			["xi.clock"] = "xi-core/clock.lua",
			["xi.bitop"] = "xi-core/bitop.lua",
			["xi.theory.euclid"] = "xi-theory/euclid.lua",
			["xi.theory.scales"] = "xi-theory/scales.lua",
			["xi.theory.chords"] = "xi-theory/chords.lua",
			["xi.pattern_factory"] = "xi-core/pattern_factory.lua",
			["xi.mini.grammar"] = "xi-mini/grammar.lua",
			["xi.mini.visitor"] = "xi-mini/visitor.lua",
		},
	},
	-- bin = {"bin/xi"}
}
