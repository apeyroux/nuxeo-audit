with import <nixpkgs> {};

let
  myapp = haskellPackages.callCabal2nix "nuxeo-audit" ./. {};
in
  dockerTools.buildImage {
    name = "myapp";
    tag = "latest";
    created = "now";
    contents = [ myapp ];
    config = {
      EntryPoint = ["nuxeo-audit"];
      Cmd = ["nuxeo-audit"];
   };
 }
