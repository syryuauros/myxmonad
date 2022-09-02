{
  description = "my xmonad configurations";

  inputs = {
    haedosa.url = "github:haedosa/flakes";
    nixpkgs.follows = "haedosa/nixpkgs";
    home-manager.follows = "haedosa/home-manager";
    wallpapers.url = "github:jjdosa/wallpapers";
    wallpapers.flake = false;
  };

  outputs = inputs@{ self, ... }:
    let
      system = "x86_64-linux";
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      };
    in with pkgs; {
      overlay = import ./overlay.nix;
      hmModule = import ./hmModule.nix inputs.wallpapers;
      devShell.${system} = mkShell { buildInputs = with pkgs; [ xmonad-with-packages ]; };
      packages.${system}.default = xmonadBin;
      apps.${system}.default = {
        type = "app";
        program = "${xmonad-restart}/bin/xmonad-restart";
      };
    };

}
