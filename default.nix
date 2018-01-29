{ pkgs ? import <nixpkgs> {} }:
  pkgs.haskellPackages.callPackage ./c-dsl.nix {}
