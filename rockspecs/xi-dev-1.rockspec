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
			["xi.init"] = "src/xi/init.moon",
			["xi.utils"] = "src/xi/utils.moon",
			["xi.fraction"] = "src/xi/fraction.moon",
			["xi.drawline"] = "src/xi/drawline.moon",
			["xi.span"] = "src/xi/span.moon",
			["xi.event"] = "src/xi/event.moon",
			["xi.state"] = "src/xi/state.moon",
			["xi.stream"] = "src/xi/stream.moon",
			["xi.pattern"] = "src/xi/pattern.moon",
			["xi.control"] = "src/xi/control.moon",
			["xi.clock"] = "src/xi/clock.moon",
			["xi.bitop"] = "src/xi/bitop.moon",
			["xi.theory.euclid"] = "src/xi/theory/euclid.moon",
			["xi.theory.scales"] = "src/xi/theory/scales.moon",
			["xi.theory.chords"] = "src/xi/theory/chords.moon",
			["xi.pattern_factory"] = "src/xi/pattern_factory.moon",
			["xi.mini.grammar"] = "src/xi/mini/grammar.moon",
			["xi.mini.visitor"] = "src/xi/mini/visitor.moon",
		},
	},
	-- install = {
	--    bin = {"bin/xi"}
	-- }
}
