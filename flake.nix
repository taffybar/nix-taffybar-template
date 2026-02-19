{
  description = "Minimal project-style Taffybar template with Nix flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    taffybar = {
      url = "github:taffybar/taffybar";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
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

      hpkgs = pkgs.haskellPackages.override {
        overrides = hself: _: {
          nix-taffybar-template = hself.callCabal2nix "nix-taffybar-template" ./. {};
        };
      };

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
}
