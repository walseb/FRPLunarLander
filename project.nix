{ nixpkgs ? import <nixpkgs> {}, mkDerivation, base, linear, reactive-banana, sdl2, sdl2-image, sdl2-sprite, stdenv, lens, monad-loops, cabal-install, extra }:
let
  inherit (nixpkgs) pkgs;
in
mkDerivation {
  pname = "ReactiveGame";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base linear reactive-banana sdl2 sdl2-image sdl2-sprite lens monad-loops cabal-install extra pkgs.haskellPackages.reactive-banana-sdl2-bsd
  ];
  license = stdenv.lib.licenses.bsd3;
}
