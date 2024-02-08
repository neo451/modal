# xi

[![Install](https://img.shields.io/badge/Install-LuaRocks-brightgreen?style=for-the-badge&color=violet)](https://luarocks.org/modules/neo451/xi)
[![Docs](https://img.shields.io/github/actions/workflow/status/noearc/xi/docs.yml?style=for-the-badge&label=docs)](https://noearc.github.io/xi/)
[![Tests](https://img.shields.io/github/actions/workflow/status/noearc/xi/test.yml?style=for-the-badge&label=tests)]
![License](https://img.shields.io/github/license/noearc/xi?style=for-the-badge)
![Version](https://img.shields.io/luarocks/v/neo451/xi?style=for-the-badge)


moonscript port of tidal, this project is at early alpha stage, all kinds of things can go wrong, but trying out and feedbacks are appreciated!

## install
1. install luarocks
2. `luarocks install xi`

## building
1. clone or download the project zip from github
2. open a terminal in the project directory
3. run `sudo ./actions/build.sh`

## trying out

1. start supercollider and (in supercollider) run `SuperDirt.start`
2. get [xi.nvim](https://github.com/noearc/xi.nvim) to try in neovim, write a text file (see example.yue), then run `moon boot.moon` in a terminal, then start evaluating with `Ctrl-a` (send-all) or `Ctrl-e` (send-current-line)
3. or run `moon repl.moon` and `xi` in two terminal windows

## Collaboration
Collaboration is welcome!
