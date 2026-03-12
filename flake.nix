{
  description = "A structural Nix code formatter.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = {nixpkgs, ...}: let
    eachSystem = f: nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (x: f nixpkgs.legacyPackages.${x});
  in {
    packages = eachSystem (pkgs: rec {
      default = nestix;

      nestix = pkgs.rustPlatform.buildRustPackage {
        name = "nestix";
        src = ./.;
        cargoLock.lockFile = ./Cargo.lock;
      };
    });

    devShells = eachSystem (pkgs: {default = pkgs.mkShell {packages = with pkgs; [alejandra cargo rustfmt treefmt];};});

    formatter = eachSystem (pkgs: pkgs.treefmt);
  };
}
