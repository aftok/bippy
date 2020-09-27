let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  inherit (pkgs) haskellPackages;

  libInputs = [
    pkgs.secp256k1
  ];
in
pkgs.stdenv.mkDerivation{
  name = "bippy";

  buildInputs = [
    pkgs.ghc
    pkgs.pkg-config
    haskellPackages.cabal-install
  ] ++ libInputs;

  LD_LIBRARY_PATH = "${pkgs.stdenv.lib.makeLibraryPath libInputs}";
}
