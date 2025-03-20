inputs: { config, lib, pkgs, ... }:

with lib;

let

  cfg = config.mysystem.windowManager.xmonad;

  xmobarrc = pkgs.callPackage ../../packages/xmobar {};

in {

  options = {
    mysystem.windowManager.xmonad = {
      enable = mkEnableOption "xmonad window manager";
      config = mkOption {
        type = types.nullOr types.path;
        default = ../../xmonad/xmonad.hs;
        example = literalExpression ''
          pkgs.writeText "xmonad.hs" '''
            import XMonad
            main = xmonad defaultConfig
                { terminal    = "urxvt"
                , modMask     = mod4Mask
                , borderWidth = 3
                }
          '''
        '';
      };
      xmobarrc = mkOption {
        type = types.nullOr types.path;
        default = xmobarrc;
      };

    };
  };

  config = mkIf cfg.enable {

    xsession.enable = true;
    xsession.windowManager.xmonad = {
      config = cfg.config;
      enable = true;
      enableContribAndExtras = true;
    };

    programs.home-manager.enable = true;
    xdg.enable = true;

    xdg.configFile."xmobar/xmobarrc".source = mkIf (cfg.xmobarrc != null) cfg.xmobarrc;

    home.packages = with pkgs; [
      flameshot
      xmobar
      dmenu
      arandr
      picom
    ];

    services.random-background = {
      enable = true;
      imageDirectory = "${inputs.wallpapers}";
      display = "fill";
      interval = "0.3h";
    };

  };

}
