#!/bin/echo You must source this file with your shell: $ source

pushd `dirname "$0"` >/dev/null

echo "Add to PATH: `pwd`/bin"
export PATH="$PATH:`pwd`/bin"

echo "Add to C_INCLUDE_PATH: `pwd`/src/lib"
export C_INCLUDE_PATH="$C_INCLUDE_PATH:`pwd`/src/lib"

echo "Add to GPR_PROJECT_PATH: `pwd`"
export GPR_PROJECT_PATH="$GPR_PROJECT_PATH:`pwd`"

echo "Add to LIBRARY_PATH: `pwd`/lib/debug"
export LIBRARY_PATH="$LIBRARY_PATH:`pwd`/lib/debug"

echo "Add to LD_LIBRARY_PATH: `pwd`/lib/debug"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:`pwd`/lib/debug"

popd >/dev/null
