with import <nixpkgs> {};

let
  drv = haskellPackages.callCabal2nix "nuxeo-audit" ./. {};
in if lib.inNixShell then drv.env else drv
