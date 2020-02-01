{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    pkgs.cabal-install
    pkgs.ghc
    pkgs.SDL2
    pkgs.SDL2_image
    pkgs.SDL2_ttf
    pkgs.pkg-config
  ];
}
