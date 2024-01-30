# xi
moonscript port of tidal

## dependencies
You might need the following (they may have different names in your package manager):

liblua5.4-dev, openssl, libssl-dev, and libasio-dev, luarocks

## building
1. clone or download the project zip from github
2. open a terminal in the project directory
3. run the following command: `luarocks install --only-deps rockspecs/tranquility-dev-1.rockspec`
3. ./actions/build.sh (you may need sudo)

## trying out

1. start supercollider and (in supercollider) run SuperDirt.start;
2. checkout (xi.nvim)[https://github.com/noearc/xi.nvim] to try in neovim, write a text file (see example.yue), then run `moon boot.moon` in a terminal, then start evaluating with `Ctrl-a` (send-all) or `Ctrl-e` (send-current-line)
3. or run `moon repl.moon` and `moon boot.moon` in two terminal windows

## Collaboration
Collaboration is welcome! 
