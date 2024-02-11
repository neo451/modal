# LUA= $(shell echo `which lua`)
# LUA_BINDIR= $(shell echo `dirname $(LUA)`)
# LUA_PREFIX= $(shell echo `dirname $(LUA_BINDIR)`)
# LUA_SHAREDIR=$(LUA_PREFIX)/share/lua/5.1
#
# _REPODIR != cd "$(shell dirname $(firstword $(MAKEFILE_LIST)))/" && pwd

xi:

test:
	busted .

doc:
	ldoc .

remove:
	luarocks remove xi

install:
	moonc src/
	luarocks build

lint:
	moonc -l src
