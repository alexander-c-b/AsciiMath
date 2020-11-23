{ pkgs ? import ./nix/pkgs.nix {} }:

(import ./default.nix {}).env.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ [pkgs.cabal-install];
})
