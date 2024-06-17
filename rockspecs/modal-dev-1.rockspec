rockspec_format = "3.0"
package = "modal"
version = "dev-1"
source = {
   url = "https://github.com/noearc/modal.git",
}

description = {
   summary = "lua port of the tidalcycles pattern language",
   detailed = [[
modal is an experimental port of the livecoding music language Tidalcycles(http://tidalcycles.org/) to the lua.
This project follows the footsteps of vortex, strudel and tranquility.]],
   homepage = "https://github.com/noearc/modal",
   license = "GPL3",
}

dependencies = {
   "lua >= 5.1",
   "losc >= 1.0.1-1",
   "luasocket >= 3.1.0-1",
   "abletonlink >= 1.0.0-1",
   "lpeg >= 1.1.0-1",
   "readline >= 3.3-0",
}

build = {
   type = "builtin",
   modules = {
      ["modal.init"] = "src/init.lua",
      ["modal.luacats"] = "src/luacats.lua",
      ["modal.utils"] = "src/core/utils.lua",
      ["modal.log"] = "src/core/log.lua",
      ["modal.bitop"] = "src/core/bitop.lua",
      ["modal.fun"] = "src/core/fun.lua",

      ["modal.stream"] = "src/core/stream.lua",
      ["modal.drawline"] = "src/core/drawline.lua",
      ["modal.types"] = "src/core/types.lua",
      ["modal.pattern"] = "src/core/pattern.lua",
      ["modal.factory"] = "src/core/factory.lua",

      ["modal.params"] = "src/params/params.lua",
      ["modal.ui"] = "src/params/ui.lua",

      ["modal.clock"] = "src/clock/clock.lua",

      ["modal.euclid"] = "src/core/euclid.lua",
      ["modal.scales"] = "src/core/scales.lua",
      ["modal.chords"] = "src/core/chords.lua",
      ["modal.lib"] = "src/core/lib.lua",

      ["modal.maxi"] = "src/core/maxi.lua",
      ["modal.ast_to_src"] = "src/core/ast_to_src.lua",
      ["modal.typedef"] = "src/core/typedef.lua",

      ["modal.repl"] = "src/repl/repl.lua",
      ["modal.luv_repl"] = "src/repl/luv_repl.lua",
      ["modal.server"] = "src/repl/server.lua",
   },
   install = {
      bin = {
         "bin/modal",
         "bin/mods",
         "bin/lmodal",
      },
   },
}
