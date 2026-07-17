{ pkgs ? import <nixpkgs> { } }:

# Dev shell for building this Lean project against Mathlib.
# Mathlib's `cache` tool links against OpenSSL; on NixOS the linker
# can't find libssl/libcrypto unless they're provided here. Putting
# openssl in buildInputs makes the nix gcc-wrapper add the right
# -L (and RPATH) via NIX_LDFLAGS automatically.
pkgs.mkShell {
  buildInputs = [
    pkgs.openssl
    pkgs.curl   # used by `lake exe cache get` to download oleans
    pkgs.git
  ];
}
