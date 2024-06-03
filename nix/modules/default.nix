{ container-manager-pkg }: { config, pkgs, lib, ... }: {
  imports = [
    ./container-jails.nix
    ./pluto.nix
  ];

  nixpkgs.overlays = [
    (self: super: {
      container-manager = container-manager-pkg;
    })
  ];
}
