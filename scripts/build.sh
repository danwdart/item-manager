nix-build -o result/backend -A ghc.backend
nix-build -o result/frontend-ghc -A ghc.frontend
nix-build default-fix-ghcjs.nix -o result/frontend -A ghcjs.frontend
# nix-build -o result/android -A android.frontend
nix-build -o result/warp -A ghc.wai-app-static