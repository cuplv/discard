{ nixpkgs ? import <nixpkgs> {} }:

let drv = nixpkgs.pkgs.haskellPackages.callPackage ./package.nix {};
in drv.env
