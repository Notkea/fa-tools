# File part of fa-tools
# Copyright 2023 Notkea
# Licensed under the EUPL version 1.2

{
  description = "A collection of tools to download content from FurAffinity.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    flaky-utils.url = "git+https://cgit.pacien.net/libs/flaky-utils";
  };

  outputs = { self, nixpkgs, flake-utils, flaky-utils }:
  flake-utils.lib.eachDefaultSystem (system: let
    pkgs = import nixpkgs { inherit system; };
    cabalDeps = drv: with pkgs.lib; concatLists (attrValues drv.getCabalDeps);
    extraPathDeps = with pkgs; [
    ];

  in rec {
    apps = {
      fa-subs = flake-utils.lib.mkApp {
        name = "fa-subs";
        drv = packages.default;
      };
    };

    packages.default = pkgs.haskell.lib.compose.overrideCabal (super: {
      buildTools = (super.buildTools or []) ++ (with pkgs; [
        makeWrapper
      ]);

      postInstall = ''
        ${super.postInstall or ""}

        # wrapper for runtime dependencies registration
        for executable in "$out/bin/"*; do
          wrapProgram "$executable" \
            --prefix PATH : ${pkgs.lib.makeBinPath extraPathDeps}
        done

        # bash completion
        compl_dir="$out/share/bash-completion/completions"
        mkdir -p "$compl_dir"
        for executable in "$out/bin/"*; do
          "$executable" --help=bash \
            > "$compl_dir/$(basename "$executable")"
        done

        # manual page
        mkdir -p "$out/share/man/man1"
        ${pkgs.pandoc}/bin/pandoc --standalone --to man \
          readme.md \
          --output "$out/share/man/man1/fa-tools.1"
      '';
    }) (pkgs.haskellPackages.callCabal2nix "fa-tools" ./. { });

    devShells.default = flaky-utils.lib.mkDevShell {
      inherit pkgs;
      tools = with pkgs; [
        (haskellPackages.ghcWithHoogle (ps: with ps; [
          cabal-install
          hlint
          apply-refact
          pandoc
        ] ++ (cabalDeps packages.default)))
      ] ++ extraPathDeps;
    };
  });
}
