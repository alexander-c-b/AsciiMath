{ pkgs ? import ./nix/pkgs.nix {}, compiler ? "ghc865" }:

pkgs.haskell.packages.${compiler}.callCabal2nix "AsciiMath" ./. {}
