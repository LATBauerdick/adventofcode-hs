{
  description = "Advent of Code 2024";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem ( system :
        let
          aoc = import ./package.nix { inherit system; };
        in rec {
          devShells.default = import ./shell.nix { inherit system; };
          packages.default = aoc;
          apps.default = {
            type = "app";
            program = "${aoc}/bin/aoc";
          };
        }
    );
}
