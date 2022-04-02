{
  description = "Handle exceptions thrown in IO with fused-effects.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-compat, flake-utils }:
    let
      name = "fused-effects-exceptions";
    in
    {
      overlay = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = prev.lib.composeExtensions
            (old.overrides or (_: _: {}))
            (_self: _super: {
              "${name}" = final.haskellPackages.callCabal2nix name self { };
            });
        });
      };
    } // (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        };

        haskell = {
          ghc = pkgs.haskellPackages.ghc;
          hls = pkgs.haskell-language-server.override {
            supportedGhcVersions = [ "8107" ];
          };
        };
      in
      with pkgs; {
        packages = {
          "${name}" = pkgs.haskellPackages.callCabal2nix name self { };
        };

        defaultPackage = self.packages.${system}.${name};

        devShell = mkShell {
          buildInputs = [
            haskell.ghc
            haskell.hls
            gdb
            haskellPackages.cabal-install
            haskellPackages.stylish-haskell
          ];
        };
      }));
}
