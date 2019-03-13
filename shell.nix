let
  nixPinned = import (builtins.fetchTarball  "https://github.com/NixOS/nixpkgs/archive/fa82ebccf66.tar.gz") {};
in
  { nixpkgs ? nixPinned }:
    let
      venula = (import ./default.nix { inherit nixpkgs; });
      venulaShell = with nixpkgs;
      haskell.lib.overrideCabal venula (oldAttrs: {
        librarySystemDepends = with pkgs; [
          cabal-install
          haskellPackages.ghcid
          sourceHighlight
        ];
      });
    in
      venulaShell.env
