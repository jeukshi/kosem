{
  description = "Kosem - typed SQL and relational mapping for Haskell";
  inputs = {
    # nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # Use master for GHC 9.6 and HLS 2.6 this is temporary.
    nixpkgs.url = "github:nixos/nixpkgs/master";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', inputs', pkgs, system, config, ... }: {

        haskellProjects.default = {
          devShell = {
           enable = true;
           tools = hp: { fourmolu = hp.fourmolu; };

           hlsCheck.enable = true;
          };

          autoWire = [ "packages" "apps" "checks" ];
        };
        packages.default = self'.packages.kosem-postgresql;

        devShells.default = pkgs.mkShell {
          name = "Kosem";
          meta.description = "Kosem dev shell";
          # See https://zero-to-flakes.com/haskell-flake/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            #config.treefmt.build.devShell
          ];
          nativeBuildInputs = with pkgs; [
            just
            postgresql
          ];
          shellHook = ''
             cat scripts/shell-welcome.txt
          '';


        };

      };
    };
}
