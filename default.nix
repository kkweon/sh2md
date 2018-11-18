{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.haskellPackages.callPackage ./sh2md.nix { }
