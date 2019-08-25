{ pkgs ? import <nixpkgs> {}, ...}:

with pkgs;

{
  bookcopy = haskellPackages.callPackage ./bookcopy/default.nix {};
}
