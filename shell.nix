{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/383b3b36d06bddd730acbb466f66af5b7bee913a.tar.gz") {} }:
let
  inherit (pkgs) haskellPackages;

  project = import ./release.nix { pkgs = pkgs; };
in
pkgs.mkShell {
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.haskell-language-server
    pkgs.ormolu
  ];
}
