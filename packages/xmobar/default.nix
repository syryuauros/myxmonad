{ substitute }:
substitute {
  src = ./xmobarrc;
  replacements = [
    [ "--replace" "@iconRoot@" "${./xpm}" ]
  ];
}
