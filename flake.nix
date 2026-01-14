{
  description = "A structural Nix code formatter.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.treefmt = {
    url = "github:numtide/treefmt-nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    nixpkgs,
    treefmt,
    ...
  }: let
    eachSystem = f: nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (x: f nixpkgs.legacyPackages.${x});

    formatter = pkgs:
      (treefmt.lib.evalModule pkgs {
        programs.alejandra.enable = true;
        programs.rustfmt.enable = true;
      }).config.build.wrapper;
  in {
    packages = eachSystem (pkgs: rec {
      default = nestix;

      nestix = pkgs.rustPlatform.buildRustPackage {
        name = "nestix";
        src = ./.;
        cargoLock.lockFile = ./Cargo.lock;
      };
    });

    devShells = eachSystem (pkgs: {default = pkgs.mkShell {packages = [pkgs.cargo (formatter pkgs)];};});

    formatter = eachSystem formatter;
  };
}
