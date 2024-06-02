# modal

lua port of tidal, this project is at early alpha stage, all kinds of things can go wrong, but trying out and feedbacks are appreciated!

## Install

1. install luarocks
2. `luarocks install modal` **not ready yet**

## Building

1. clone or download the project zip from github
2. open a terminal in the project directory
3. run `sudo make install`

## Trying out

1. start supercollider and (in supercollider) run `SuperDirt.start`
2. get [modal.nvim](https://github.com/noearc/modal.nvim)
3. or in terminal, launch `mods` (server backend) and `modal` (repl)

## History

This porject works on top of the working prototype port of [tranquility]( https://github.com/XiNNiW/tranquility ), the og lua port. I originally intended this as a moonscript port, but as this one went larger and larger, lua tooling is obviously better. plus I wrote the custom parser to replace it as the user code parser. So this project is the current lua port of Tidal that went the furtherest for now :)

## Collaboration

Collaboration is welcome!
