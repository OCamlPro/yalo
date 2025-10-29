#!/bin/sh

echo ./_build/default/src/yalo/main.exe doc --dir docs/lints
opam exec -- ./_build/default/src/yalo/main.exe doc --dir docs/lints
