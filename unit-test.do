exec >&2

echo "-----------------------------------------------------"
echo "--  Unit tests are not maintained, they may fail.  --"
echo "-----------------------------------------------------"

redo bin/xreq.dbg

export BUILD_MODE=debug

redo-ifchange conf/flags tools/redo_gprbuild
. conf/flags
. tools/redo_gprbuild

GPR_FLAGS="-Xmode=$BUILD_MODE"
redo_gprbuild old_unit_tests.gpr "$3"

