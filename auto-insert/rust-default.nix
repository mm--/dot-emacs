{ pkgs ? import <nixpkgs> {} }:

with pkgs; {
  myenv = buildEnv {
    name = "RustEnv";
    paths = [
      rustc
      cargo
      gcc
      rustfmt
      rustPackages.clippy
    ];
  };
}
