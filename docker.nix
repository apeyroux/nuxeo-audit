with import <nixpkgs> {};

let
  nuxeo-audit = haskellPackages.callCabal2nix "nuxeo-audit" ./. {};
in rec {
  docker-img = dockerTools.buildImage {
    name = "nuxeo-audit";
    tag = "latest";
    created = "now";
    contents = [ nuxeo-audit ];
    config = {
      EntryPoint = ["nuxeo-audit"];
      Cmd = ["nuxeo-audit"];
   };
  };

  src-tgz = releaseTools.sourceTarball {
    buildInputs = [ cabal-install ];
    distPhase = ''
    cabal sdist
    mkdir -p $out/tarballs/
    cp dist/${nuxeo-audit.name}.tar.gz $out/tarballs/
    '';
    src = ./.;
  };

 }
