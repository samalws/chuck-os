{ pkgs ? (import <nixpkgs> {}).pkgsCross.i686-embedded }:
pkgs.mkShell {
  nativeBuildInputs = [ pkgs.pkgsBuildBuild.gcc pkgs.pkgsBuildBuild.qemu pkgs.pkgsBuildBuild.bintools pkgs.pkgsBuildTarget.nasm ];
}

