# modal

Lua port of tidal, this project is at early alpha stage, all kinds of things can go wrong, but trying out and feedbacks are appreciated!

See [Wiki](https://github.com/noearc/modal/wiki) for more desgin highlights and future plans.

## Dependencies

May need to manually install `libasio-dev` for abletonlink to work

## Install

### Copy

If you are familiar with lua and just want to experiment right away, copy [modal.lua](https://github.com/noearc/modal/blob/main/modal.lua) and use seqerate lua instance to run repl and server.
Install or build missing dependencies ...

### Luarocks

Install luarocks **this one is not ready yet**

```
sudo luarocks install modal 
```

### Luvit

1. Install [lit](https://github.com/luvit/lit) and

```
lit install noearc/modal
```

this installs modal.lua into a deps/ directory. later should be able to build a binary with lit as well.

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

This porject works on top of the working prototype port of [tranquility](https://github.com/XiNNiW/tranquility), the og lua port. I originally intended this as a moonscript port, because it has a more tarse syntax. But as this project grew larger, lua tooling is obviously better. Plus I wrote the custom parser to replace it as the user code parser. So this project is the current lua port of Tidal that went the furtherest for now :)

## Collaboration

Collaboration is welcome!
