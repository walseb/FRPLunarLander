{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, linear, reactive-banana, sdl2
      , sdl2-image, sdl2-sprite, stdenv, lens, monad-loops, cabal-install, extra
      }:
        mkDerivation {
          pname = "Project9";
          version = "0.1.0.0";
          src = ./.;
          isLibrary = false;
          isExecutable = true;
          executableHaskellDepends = [
            base linear reactive-banana sdl2 sdl2-image sdl2-sprite lens monad-loops cabal-install extra
          ];
          license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
