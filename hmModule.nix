wallpapers: { config, lib, pkgs, ... }:

with lib;

let

  cfg = config.mysystem.windowManager.xmonad;

  xmobarrc = pkgs.substitute {
    src = ./xmobarrc;
    replacements = [[ "--replace" "@iconRoot@" "${./xpm}" ]];
  };

in {

  options = {
    mysystem.windowManager.xmonad = {
      enable = mkEnableOption "xmonad window manager";
      config = mkOption {
        type = types.nullOr types.path;
        default = ./xmonad.hs;
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

    xdg.configFile."xmobar/xmobarrc".source = xmobarrc;

    home.packages = with pkgs; [
      xmobar
      dmenu
      arandr
    ];

    services.random-background = {
      enable = true;
      imageDirectory = "${wallpapers}";
      display = "fill";
      interval = "1h";
    };

  };

}
