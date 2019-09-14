{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
  inherit (lib) makeLibraryPath;
  hs = haskell.packages.ghc864;
  tools = [
  ];
  libraries = [
    SDL2
    SDL2_image
    pkg-config
  ];
  libraryPath = "${makeLibraryPath libraries}";
in
  pkgs.runCommand "shell" {
    buildInputs = tools ++ libraries;
    shellHook = ''
      export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${libraryPath}"
      export LIBRARY_PATH="${libraryPath}"
    '';
  } ""
