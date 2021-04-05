{ system ? builtins.currentSystem, nixpkgs ? import <unstable> {} }:
(import ./external/reflex-platform {
  inherit system;
  config.android_sdk.accept_license = true;  
}).project ({ pkgs, ... }: {
  packages = {
    backend = ./app/backend;
    frontend = ./app/frontend;
    common = ./app/common;
  };

  shellToolOverrides = ghc: super: {
    inherit (ghc) apply-refact stylish-haskell wai-app-static;
    # wai-app-static = pkgs.haskellPackages.wai-app-static;
  };

  overrides = self: super: {
    # Remove when using ghc wai/warp
    # mmorph = self.callHackage "mmorph" "1.1.3" {};

    # todo no doctests on later ghc
    # ghc = nixpkgs.haskell.compiler.ghc884; # because some of these libraries are not yet ready for 901?
    # backend = self.callCabal2nix "backend" ./backend {};
    servant-quickcheck = self.callHackage "servant-quickcheck" "0.0.7.4" {}; # 0.0.10.0
  };

  useWarp = true;

  android.frontend = {
    executableName = "frontend";
    applicationId = "com.jolharg.item-manager";
    displayName = "Item Manager";
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})
