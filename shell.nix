
{ pkgs ? import <nixpkgs> {}}: with pkgs; 

mkShell {
  nativeBuildInputs = [
      cabal2nix
      ghcid
    ];
  }
