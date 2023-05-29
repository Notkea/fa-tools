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

  in rec {
    apps = {
      fa-subs = flake-utils.lib.mkApp {
        name = "fa-subs";
        drv = packages.default;
      };
      fa-notes = flake-utils.lib.mkApp {
        name = "fa-notes";
        drv = packages.default;
      };
    };

    packages.default = pkgs.haskell.lib.compose.overrideCabal (super: {
      buildTools = (super.buildTools or []) ++ (with pkgs; [
        makeWrapper
      ]);

      postInstall = ''
        ${super.postInstall or ""}

        # bash completion
        compl_dir="$out/share/bash-completion/completions"
        mkdir -p "$compl_dir"
        for executable in "$out/bin/"*; do
          "$executable" --help=bash \
            > "$compl_dir/$(basename "$executable")"
        done

        # manual pages
        for manual_source in *.?.md; do
          page_name="$(basename "$manual_source" .md)"
          section="''${page_name##*.}"
          man_out_dir="$out/share/man/man$section"
          mkdir -p "$man_out_dir"
          ${pkgs.pandoc}/bin/pandoc --standalone --to man \
            "$manual_source" \
            --output "$man_out_dir/$page_name"
        done
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
      ];
    };
  });
}
