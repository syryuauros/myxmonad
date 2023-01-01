{
  description = "my xmonad configurations";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    wallpapers.url = "github:jjdosa/wallpapers";
    wallpapers.flake = false;
  };

  outputs = inputs@{ nixpkgs, ... }:
  let
    inherit (nixpkgs.lib) genAttrs;
    supportedSystems = [ "x86_64-linux" ];
    forAllSystems = genAttrs supportedSystems;
    pkgsFor = system: import ./pkgs.nix { inherit inputs system; };
  in
  {
    homeManagerModules = import ./modules/home-manager inputs;

    packages = forAllSystems (import ./packages inputs);

    devShells = forAllSystems (system: {
      default = import ./shell.nix { pkgs = pkgsFor system; };
    });

  };

}
