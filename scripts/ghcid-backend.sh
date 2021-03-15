#!/usr/bin/env nix-shell
#! nix-shell ../shell.nix -i bash

cabal exec ghcid -- -c "cabal repl backend" \
    --restart app/common/common.cabal \
    --restart app/backend/backend.cabal \
    --reload app/common/src \
    -aT Main.main
