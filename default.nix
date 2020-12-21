{ pkgs ? import ./nix/pkgs.nix {}, compiler ? "ghc865" }:

pkgs.haskellPackages.callCabal2nix "AsciiMath" ./. {}
