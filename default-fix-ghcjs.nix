{ system ? builtins.currentSystem }:
(import ./external/reflex-platform {
  inherit system;
  config.android_sdk.accept_license = true;  
}).project ({ pkgs, ... }: {
  packages = {
    common = ./app/common;
    backend = ./app/backend;
    frontend = ./app/frontend;
  };

  shellToolOverrides = ghc: super: {
    # inherit (ghc) wai-app-static;
    # wai-app-static = pkgs.haskellPackages.wai-app-static;
  };

  overrides = self: super: {
    # Remove when using ghc wai/warp
    mmorph = self.callHackage "mmorph" "1.1.3" {}; 
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
