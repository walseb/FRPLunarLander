let
  pkgs = import <nixpkgs> {};
in
{
  packageOverrides = super: let self = super.pkgs; in
  {
    haskellPackages = super.haskellPackages.override {
      overrides = self: super: {
        reflex = self.callPackage ./reflex {};
      };
    };
  };
}
