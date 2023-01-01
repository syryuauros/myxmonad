{ inputs ?
  let flake = builtins.getFlake (toString ./.);
  in flake.inputs
, system ? builtins.currentSystem
, overlays ? []
}:
let
  inherit (inputs) nixpkgs;
in
import nixpkgs {
  inherit system;
  overlays = [  ] ++ overlays;
}
