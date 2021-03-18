#!/usr/bin/env nix-shell
#! nix-shell ../shell.nix -i bash

# Using "all" alone fails because it cannot find the test program

cabal test common --enable-coverage
cabal test all --enable-coverage