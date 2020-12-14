{ pkgs ? import ./nix/pkgs.nix {}, compiler ? "ghc865" }:

let
  walk-ast = pkgs.fetchFromGitHub {
    owner  = "zandroidius";
    repo   = "walk-ast";
    rev    = "624fddb77bac1ca3cead3b80cfaec3483ea3e512";
    sha256 = "095af1fz61mc9qlaa2v3xzkhfbak3s93fmhcv0d9jq5734c9ccwk";
  };
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: { 
      walk-ast = self.callPackage walk-ast {};
    };
  };
in haskellPackages.callCabal2nix "AsciiMath" ./. {}
