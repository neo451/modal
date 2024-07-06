# LUA= $(shell echo `which lua`)
# LUA_BINDIR= $(shell echo `dirname $(LUA)`)
# LUA_PREFIX= $(shell echo `dirname $(LUA_BINDIR)`)
# LUA_SHAREDIR=$(LUA_PREFIX)/share/lua/5.1
#
# _REPODIR != cd "$(shell dirname $(firstword $(MAKEFILE_LIST)))/" && pwd

modal:

test:
	# sudo luarocks-5.1 build && busted --lua=/usr/bin/lua5.1
	# sudo luarocks-5.2 build && busted --lua=/usr/bin/lua5.2
	# sudo luarocks-5.3 build && busted --lua=/usr/bin/lua5.3
	sudo luarocks build --lua-version 5.4 && busted --lua=/usr/bin/lua5.4

build:
	luajit ./scripts/pack.lua > modal.lua
	echo "#!/usr/bin/luajit\nrequire'modal'.repl()" > modal
	echo "#!/usr/bin/luajit\nrequire'modal'.server()" > mods
	chmod +x modal
# doc:
# 	ldoc .

remove:
	luarocks remove modal

install:
	luarocks build
