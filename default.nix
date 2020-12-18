{ pkgs ? import ./nix/pkgs.nix {}, compiler ? "ghc865" }:

let
  walk-ast = pkgs.fetchFromGitHub {
    owner  = "zandroidius";
    repo   = "walk-ast";
    rev    = "4c671526150f9709d0fb4b73bb26b7db630bf3b6";
    sha256 = "1qfgcrp6nb6i8qlbhc8ikj1c869wkbyq294klv3a8q9pxbmibi5g";
  };
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: { 
      walk-ast = self.callPackage walk-ast {};
    };
  };
in haskellPackages.callCabal2nix "AsciiMath" ./. {}
