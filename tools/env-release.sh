#!/bin/echo You must source this file with your shell: $ source

pushd "`dirname "$0"`/.." >/dev/null

echo "Add to PATH:             `pwd`/bin"
export PATH="`pwd`/bin:$PATH"

echo "Add to C_INCLUDE_PATH:   `pwd`/src/lib"
export C_INCLUDE_PATH="`pwd`/src/lib:$C_INCLUDE_PATH"

echo "Add to GPR_PROJECT_PATH: `pwd`"
export GPR_PROJECT_PATH="`pwd`:$GPR_PROJECT_PATH"

echo "Add to LIBRARY_PATH:     `pwd`/lib/release"
export LIBRARY_PATH="`pwd`/lib/release:$LIBRARY_PATH"

echo "Add to LD_LIBRARY_PATH:  `pwd`/lib/release"
export LD_LIBRARY_PATH="`pwd`/lib/release:$LD_LIBRARY_PATH"

popd >/dev/null
