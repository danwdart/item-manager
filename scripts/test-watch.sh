#!/usr/bin/env nix-shell
#! nix-shell ../shell.nix -i bash

cabal exec ghcid -- -c "cabal repl $1-test" \
    --restart app/$1/$1.cabal \
    --reload app/common \
    --reload app/$1 \
    -aT Main.main
