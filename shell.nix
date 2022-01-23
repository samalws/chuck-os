{ pkgs ? (import <nixpkgs> {}).pkgsCross.i686-embedded }:
pkgs.mkShell {
  nativeBuildInputs = [ pkgs.pkgsBuildBuild.qemu pkgs.pkgsBuildBuild.bintools pkgs.pkgsBuildTarget.nasm ];
}

