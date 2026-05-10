# LUA= $(shell echo `which lua`)
# LUA_BINDIR= $(shell echo `dirname $(LUA)`)
# LUA_PREFIX= $(shell echo `dirname $(LUA_BINDIR)`)
# LUA_SHAREDIR=$(LUA_PREFIX)/share/lua/5.1
#
# _REPODIR != cd "$(shell dirname $(firstword $(MAKEFILE_LIST)))/" && pwd

modal:

.PHONY: lint
lint: ## Lint the code with selene and typos
	selene --config selene/config.toml src/
	typos lua

LUA ?= lua
LUA_VERSION := $(shell $(LUA) -e 'print(_VERSION:match("%d+%.%d+"))')

.PHONY: test
test: build
	LUA_PATH="./?.lua;./?/init.lua;$$(luarocks --lua-version=$(LUA_VERSION) path --lr-path 2>/dev/null);;" \
	LUA_CPATH="./?.so;$$(luarocks --lua-version=$(LUA_VERSION) path --lr-cpath 2>/dev/null);;" \
	busted --lua=$(LUA)

.PHONY: test-file
test-file: build
	LUA_PATH="./?.lua;./?/init.lua;$$(luarocks --lua-version=$(LUA_VERSION) path --lr-path 2>/dev/null);;" \
	LUA_CPATH="./?.so;$$(luarocks --lua-version=$(LUA_VERSION) path --lr-cpath 2>/dev/null);;" \
	busted --lua=$(LUA) $(FILE)

build:
	lua ./scripts/pack.lua > modal.lua
	printf "#!/usr/bin/lua\nrequire'modal'.repl()" > modal
	printf "#!/usr/bin/lua\nrequire'modal'.server()" > mods
	chmod +x modal
# doc:
# 	ldoc .

remove:
	luarocks remove modal

install:
	luarocks build
