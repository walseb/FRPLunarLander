let
  unstable = import (fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { };
in
{ nixpkgs ? import <nixpkgs> { config = import ./override.nix; }, mkDerivation, base, linear, sdl2, sdl2-image, sdl2-sprite, stdenv, lens, monad-loops, cabal-install, extra }:
# with nixpkgs; mkShell {
#   buildInputs = [ hello unstable.elmPackages.elm ];
# }
let
  inherit (nixpkgs) pkgs;
  # pkgs.dependent-sum.override { version = "0.6"; };
in
mkDerivation {
  pname = "ReactiveGame";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base linear sdl2 sdl2-image sdl2-sprite lens monad-loops cabal-install extra
    # unstable.haskellPackages.dependent-map
    # unstable.haskellPackages.witherable

    # pkgs.config.haskellPackages.hlint

    pkgs.config.haskellPackages.reflex
    # pkgs.config.haskellPackages.witherable
    # pkgs.config.haskellPackages.dependent-sum
    # unstable.haskellPackages.ghc-lib-parser
    # unstable.haskellPackages.constraints-extras
    # unstable.haskellPackages.dependent-sum
  ];
  license = stdenv.lib.licenses.bsd3;
}
