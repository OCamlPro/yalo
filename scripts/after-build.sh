#!/bin/sh

set -x

INCLUDES="-I ./_build/install/default/lib/yalo_plugin_ocaml -I ./_build/install/default/lib/yalo_plugin_YALO -I ./_build/install/default/lib/yalo_plugin_FIND -I ./_build/install/default/lib/yalo_lib -I share"


echo ./_build/default/src/yalo/main.exe ${INCLUDES} doc --dir docs/lints
opam exec -- ./_build/default/src/yalo/main.exe ${INCLUDES} doc --dir docs/lints
