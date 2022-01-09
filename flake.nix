# File part of fa-scripts
# Copyright 2021 Notkea
# Licensed under the EUPL version 1.2

{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachDefaultSystem (system:
  with import nixpkgs { inherit system; };
  let
    python = python39;
    pythonPackages = python39Packages;

    pythonDependencies = pythonPackages: with pythonPackages; [
      requests
      ratelimiter
      beautifulsoup4
      html5lib
      (buildPythonPackage rec {
        pname = "markdownify";
        version = "0.9.2";
        src = fetchPypi {
          inherit pname version;
          sha256 = "sha256-uXWWiZVf1Wzfhs+WLSi1FkQ3IMOTR5FdvZoMN3n7130=";
        };
        buildInputs = [ flake8 ];
        propagatedBuildInputs = [ beautifulsoup4 six ];
        checkInputs = [ pytest ];
        pythonImportsCheck = [ pname ];
      })
    ];

    exposeScripts = with lib; flip genAttrs (name: {
      type = "app";
      program = "${self.defaultPackage.${system}}/bin/${name}.py";
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

    defaultPackage = stdenv.mkDerivation rec {
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
      "get-submission"
    ];
  });
}
