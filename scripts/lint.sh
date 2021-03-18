#!/usr/bin/env nix-shell
#! nix-shell ../shell.nix -i bash

for FILE in $(find app -name *.hs)
    do
    echo Fixing $FILE
    hlint --refactor-options=-i --refactor $FILE
    stylish-haskell -i $FILE
done