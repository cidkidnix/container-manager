{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs: let
    lib = inputs.nixpkgs.lib;
    project = self: self.callCabal2nix "container-manager" ./. {};
    haskellPackages = pkgs: pkgs.haskell.packages.ghc948.override {
      overrides = self: super: {
        container-manager = project self;
      };
    };
    supportedSystems = lib.genAttrs
      [ "x86_64-linux"
        "aarch64-linux"
      ];
  in {
    packages = supportedSystems (system: let
      pkgs = inputs.nixpkgs.legacyPackages."${system}";
    in { default = (haskellPackages pkgs).container-manager; });

    nixosModules = supportedSystems (system: {
      default = import ./nix/modules/default.nix {
        container-manager-pkg = inputs.self.packages."${system}".default;
      };
    });

    devShells = supportedSystems (system: let
      pkgs = inputs.nixpkgs.legacyPackages."${system}";
      hsPkgs = haskellPackages pkgs;
    in {
      default = hsPkgs.shellFor {
        packages = ps: with ps; [ container-manager ]; buildInputs = with hsPkgs; [ cabal-install ];
        shellHook = let
          ghcidWrapped = pkgs.writeShellScriptBin "ghcid" ''
            ${hsPkgs.ghcid.bin}/bin/ghcid --command "cabal repl"
          '';
          ghcidUnwrapped = pkgs.writeShellScriptBin "ghcid-unwrapped" ''
            ${hsPkgs.ghcid.bin}/bin/ghcid
          '';
        in ''
          # To find freshly-`cabal install`ed executables
          export PATH=~/.local/bin:${ghcidWrapped}/bin:${ghcidUnwrapped}/bin:$PATH
        '';
      };
      container-manager = hsPkgs.shellFor { packages = ps: with ps; [ container-manager ]; buildInputs = with hsPkgs; [ cabal-install ]; };
    });
  };
}
