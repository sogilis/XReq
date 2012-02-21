export BUILD_MODE=coverage
export LIBTYPE=static

redo-ifchange ../conf/flags ../tools/redo_gprbuild ../lib/xreqlib
. ../conf/flags
. ../tools/redo_gprbuild

GPR_FLAGS="-Xtype=$LIBTYPE -Xmode=$BUILD_MODE"
redo_gprbuild ../xreq.gpr "$3"

