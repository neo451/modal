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
   type = "command",
   build_command = "make build",
   install = {
      lua = {
         ["modal"] = "modal.lua",
      },
      bin = {
         ["modal"] = "modal",
         -- "modal",
      },
   },
}
