let config = {
  allowUnfree = true;
  packageOverrides = pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: {
        prizm = haskellPackagesNew.callPackage ./default.nix { };
      };
    };
  };
};
in

{ pkgs ? import <nixpkgs> { inherit config; } }:
let
  darwinPkgs = import <nixpkgs> { inherit config; system = "x86_64-darwin"; };
  linuxPkgs  = import <nixpkgs> { inherit config; system = "x86_64-linux" ; };
  pkgs       = import <nixpkgs> { inherit config; };

in
  { prizm-linux  =  linuxPkgs.haskellPackages.prizm;
    prizm-darwin = darwinPkgs.haskellPackages.prizm;
    prizm        =       pkgs.haskellPackages.prizm;
  }
