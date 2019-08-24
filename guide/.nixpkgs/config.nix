let
  pkgs = import <nixpkgs> {};
in
{
  packageOverrides = super: let self = super.pkgs; in
  {
    haskellPackages = super.haskellPackages.override {
      overrides = self: super: {
        reactive-banana-sdl2-bsd = self.callPackage ./reactive-banana-sdl2 {};
      };
    };
  };
}
