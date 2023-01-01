inputs: system:
let
  pkgs = import ../pkgs.nix { inherit inputs system; };

  xmobarrc = pkgs.callPackage ./xmobar {};

  xmonadBin = pkgs.runCommandLocal "xmonad-compile" {
    nativeBuildInputs = [ pkgs.xmonad-with-packages ];
  } ''
    mkdir -p $out/bin

    export XMONAD_CONFIG_DIR="$(pwd)/xmonad-config"
    export XMONAD_DATA_DIR="$(pwd)/data"
    export XMONAD_CACHE_DIR="$(pwd)/cache"

    mkdir -p "$XMONAD_CONFIG_DIR/lib" "$XMONAD_CACHE_DIR" "$XMONAD_DATA_DIR"

    cp ${../xmonad.hs} xmonad-config/xmonad.hs

    xmonad --recompile

    mv "$XMONAD_CACHE_DIR/xmonad-${pkgs.system}" $out/bin/
  '';


  xmonad-restart = pkgs.writeShellScriptBin "xmonad-restart" ''
    ${xmonadBin}/bin/xmonad-${pkgs.system} --restart
  '';

in
{
  inherit
    xmobarrc
    xmonadBin
    xmonad-restart;
}
