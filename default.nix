{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.pkgs.haskell.packages.ghc90.callPackage ./mad-props.nix { }

