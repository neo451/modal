# modal

LUA port of [tidal](https://tidalcycles.org), this project is at an early alpha stage and all kinds of things can go wrong, but trying it out and providing feedback is very much appreciated!

See [Wiki](https://github.com/noearc/modal/wiki) for more design highlights and future plans.

## Dependencies

You may need to manually install `libasio-dev` for abletonlink to work

## Install

### Copy

If you are familiar with LUA and just want to experiment right away, copy [modal.lua](https://github.com/noearc/modal/blob/main/modal.lua) and use a seqerate LUA instance to run `repl` and `server`.
Install or build missing dependencies ...

### Luarocks

Install `luarocks` **this one is not ready yet**

```
sudo luarocks install modal 
```

### Luvit

1. Install [lit](https://github.com/luvit/lit) and

```
lit install noearc/modal
```

this installs modal.lua into a `deps/` directory. You should later be able to build a binary with `lit` as well.

## Build and Develop

1. Clone or download the project zip from github
2. Open a terminal in the project directory
3. Run `sudo make install`
4. Install [busted](https://luarocks.org/modules/lunarmodules/busted) to run tests with `busted`

## Play

1. Start supercollider and run `SuperDirt.start`.
2. Use [modal.nvim](https://github.com/noearc/modal.nvim) in neovim.
3. Or in terminal, launch `mods` (server backend) and `modal` (repl) side by side.

## History

This project works on top of the working prototype port of [tranquility](https://github.com/XiNNiW/tranquility), the og LUA port. I originally intended this as a moonscript port, because it has a more terse syntax. But as this project grew larger, LUA tooling is obviously better. Plus I wrote the custom parser to replace it as the user code parser. So this project is the current LUA port of Tidal that got furthest for now :)

## Collaboration

Collaboration is welcome!
