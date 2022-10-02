
{ pkgs ? import <nixpkgs> {}}: with pkgs; 

let env = (import ./default.nix { }).env;

in mkShell {
    buildInputs = [
        (haskellPackages.ghcWithPackages (pkgs: with pkgs; [ env ]))
	cabal-install
	cabal2nix
	ghcid
    ];
  }
