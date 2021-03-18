#!/usr/bin/env nix-shell
#! nix-shell ../shell.nix -i bash

JSADDLE_WARP_PORT=8080 cabal repl frontend
