with import <nixpkgs> {};

let

  # cachix use static-haskell-nix 
  static-pkgs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/88ae8f7d.tar.gz) {};
  nuxeo-audit = haskellPackages.callCabal2nix "nuxeo-audit" ./. {};
  vmDebian = pkgs.vmTools.diskImageFuns.debian8x86_64 {};

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

  static-bin = pkgsMusl.callPackage ((fetchTarball https://github.com/apeyroux/static-haskell-nix/archive/apeyroux.tar.gz) + "/survey") {normalPkgs=static-pkgs; };

  src-tar = releaseTools.sourceTarball {
    buildInputs = [ cabal-install ];
    distPhase = ''
    cabal sdist
    mkdir -p $out/tarballs/
    cp dist/${nuxeo-audit.name}.tar.gz $out/tarballs/
    '';
    src = ./.;
  };

  # NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/88ae8f7d.tar.gz  nix-build release.nix -A bin-tar
  bin-tar = releaseTools.binaryTarball {
    doCheck = false;
    showBuildStats = false;
    buildPhase = "";
    installPhase = ''
    releaseName=${nuxeo-audit.name}
    ${coreutils}/bin/install --target-directory "$TMPDIR/inst/bin" -D ${static-bin.haskellPackages.nuxeo-audit}/bin/nuxeo-audit
    '';
    src = ./.;
  };

  # NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/88ae8f7d.tar.gz nix-build release.nix -A deb --out-link deb
  deb = releaseTools.debBuild {
    diskImage = vmDebian;
    name = nuxeo-audit.name;
    debMaintainer = "Alexandre Peyroux";
    meta.description = nuxeo-audit.name;
    doCheck = false;
    doInstallCheck = false;
    showBuildStats = false;
    preInstall = ''
cat <<EOT >> Makefile
install:
	cp ${static-bin.haskellPackages.nuxeo-audit}/bin/nuxeo-audit /usr/local/bin
EOT
    '';
    src = src-tar;
  };

 }
