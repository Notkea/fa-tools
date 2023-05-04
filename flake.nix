# File part of fa-scripts
# Copyright 2023 Notkea
# Licensed under the EUPL version 1.2

{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachDefaultSystem (system:
  with nixpkgs.legacyPackages.${system};
  let
    python = python3;
    pythonPackages = python3Packages;

    pythonDependencies = pythonPackages: with pythonPackages; [
      requests
      ratelimiter
      beautifulsoup4
      html5lib
      markdownify
    ];

    exposeScripts = with lib; flip genAttrs (name: {
      type = "app";
      program = "${self.packages.${system}.default}/bin/${name}.py";
    });

  in {
    # IPython development shell
    # Run with: `nix develop`
    devShell = mkShell {
      shellHook = "ipython; exit $?";
      buildInputs = [
        (python.withPackages (ps: [ ps.ipython ] ++ pythonDependencies ps))
      ];
    };

    packages.default = stdenv.mkDerivation rec {
      name = "fa-scripts";
      src = ./.;
      nativeBuildInputs = [ pythonPackages.wrapPython ];
      propagatedBuildInputs = pythonDependencies pythonPackages;
      installPhase = "install -Dt $out/bin *.py";
      postFixup = "wrapPythonPrograms";
    };

    # Runnable scripts
    # Run with: `nix run .#script-name`
    apps = exposeScripts [
      "list-submissions"
      "download-ebook"
      "get-submission-metadata"
    ];
  });
}
