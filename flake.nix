{
  description = "Reproducible LuaJIT + Luv development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
    in {
      devShells.default = pkgs.mkShell {
        name = "luajit-luv-dev";

        # LuaJIT + modules
        packages = [
          (pkgs.luajit.withPackages (p: [
            p.luv
            p.luarocks
            p.luasocket
            p.busted
            p.readline
          ]))

          pkgs.pkg-config
          pkgs.gcc
        ];

        # Optional: environment variables for consistency
        LUA_PATH = "$LUA_PATH;./?.lua;./?/init.lua";
        LUA_CPATH = "$LUA_CPATH;./?.so";
      };
    });
}
