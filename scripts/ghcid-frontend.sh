#!/usr/bin/env nix-shell
#! nix-shell ../shell.nix -i bash

JSADDLE_WARP_PORT=8080 DIR=$PWD cabal exec ghcid -- -c "cabal repl frontend" \
    --restart app/common/common.cabal \
    --restart app/frontend/frontend.cabal \
    --restart scripts/* \
    --reload app/common/src \
    -aT Main.main
