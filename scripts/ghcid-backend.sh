#!/usr/bin/env nix-shell
#! nix-shell ../shell.nix -i bash

cabal exec ghcid -- -c "cabal repl backend" \
    --restart app/common/common.cabal \
    --restart app/backend/backend.cabal \
    --restart scripts/* \
    --reload app/common/src \
    --reload app/backend/db/* \
    -aT Main.main
