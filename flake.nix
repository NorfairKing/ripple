{
  description = "ripple";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec?ref=flake";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest?ref=flake";
    sydtest.flake = false;
    feedback.url = "github:NorfairKing/feedback";
    feedback.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;

  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    , feedback
    , dekking
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (validity + "/nix/overlay.nix"))
          (import (feedback + "/nix/overlay.nix"))
          (import (dekking + "/nix/overlay.nix"))
        ];
      };
      pkgs = pkgsFor nixpkgs;
      mkNixosModule = import ./nix/nixos-module.nix { inherit (pkgs) ripple-server; };
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = {
        backend = pkgs.ripple-server;
        frontend = pkgs.ripple-frontend;
      };
      checks.${system} = {
        frontend = self.packages.${system}.backend;
        backend = self.packages.${system}.frontend;
        shell = self.devShells.${system}.default;
        nixos-module-test = import ./nix/nixos-module-test.nix {
          inherit (pkgs) nixosTest;
          ripple-nixos-module-factory = self.nixosModuleFactories.${system}.default;
        };
        coverage-report = pkgs.dekking.makeCoverageReport {
          name = "test-coverage-report";
          packages = [
            "ripple-server"
          ];
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
            elm-format.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "ripple-shell";
        packages = p: [ p.ripple-server ];
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          cabal-install
          elm2nix
          elmPackages.elm
          elmPackages.elm-json # Install, upgrade and uninstall Elm dependencies
          elmPackages.elm-live # a live server for development
          niv
          nodePackages.uglify-js # Further optimization of output on build
          openapi-generator-cli
          pkgs.feedback
          zlib
        ] ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
            elm-format
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook + pkgs.feedback.shellHook;
      };
      nixosModules.${system}.default = mkNixosModule { envname = "production"; };
      nixosModuleFactories.${system}.default = mkNixosModule;
    };
}
