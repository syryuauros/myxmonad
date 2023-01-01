{ pkgs ? import ./pkgs.nix {}
}:
with pkgs;
let
  myghc = ghc.withPackages (p: with p;
    [ xmonad xmonad-contrib xmonad-extras ]);
in
mkShell {
  buildInputs = [
    myghc
    haskellPackages.haskell-language-server
    haskellPackages.ghcid
  ];
}
