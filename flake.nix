{
  description = "Bippy: BIP-70 in Haskell";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-23.05;
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }: let
    overlay = final: prev: let
      jailbreakUnbreak = pkg:
        final.haskell.lib.doJailbreak (pkg.overrideAttrs (_: {meta = {};}));

      dontCheck = pkg: final.haskell.lib.dontCheck pkg;

      haskell-overlay = hfinal: hprev: {
      };
    in {
      haskellPackages = prev.haskellPackages.extend haskell-overlay;
    };
  in
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [overlay];
        };

        hspkgs = pkgs.haskellPackages;
      in {
        packages = {
          bippy = hspkgs.callCabal2nix "bippy" ./. {};
          default = self.packages.${system}.bippy;
        };

        devShells = {
          default = pkgs.mkShell {
            buildInputs = [
              hspkgs.ormolu
            ];
            nativeBuildInputs = [
              pkgs.binutils
              pkgs.secp256k1
            ];
            inputsFrom = builtins.attrValues self.packages.${system};
          };
        };

        formatter = pkgs.alejandra;
      }
    );
}
