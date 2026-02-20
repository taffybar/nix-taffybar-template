{
  description = "Minimal project-style Taffybar template with Nix flakes";

  inputs = {
    taffybar = {
      url = "github:taffybar/taffybar";
    };
    nixpkgs.follows = "taffybar/nixpkgs";
    flake-utils.follows = "taffybar/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    taffybar,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [taffybar.overlays.default];
        config.allowBroken = true;
      };

      hpkgs = pkgs.haskellPackages.extend (
        hself: hsuper: {
          # Keep this template build focused on the template package in CI.
          taffybar = pkgs.haskell.lib.dontCheck hsuper.taffybar;
          nix-taffybar-template = hself.callCabal2nix "nix-taffybar-template" ./. {};
        }
      );

      templatePackage = hpkgs.nix-taffybar-template;
    in {
      packages = {
        default = templatePackage;
        nix-taffybar-template = templatePackage;
      };

      apps.default = {
        type = "app";
        program = "${templatePackage}/bin/taffybar";
        meta.description = "Run the template taffybar binary";
      };

      checks.default = templatePackage;

      devShells.default = hpkgs.shellFor {
        packages = p: [p.nix-taffybar-template];
        nativeBuildInputs = [
          pkgs.cabal-install
          pkgs.haskell-language-server
          pkgs.ormolu
        ];
      };
    });

  nixConfig = {
    extra-substituters = ["https://haskell-language-server.cachix.org"];
    extra-trusted-public-keys = ["haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="];
  };
}
