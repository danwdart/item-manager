{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  shellToolOverrides = ghc: super: {
    inherit (ghc) hlint stylish-haskell;
  };
  useWarp = true;

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})
