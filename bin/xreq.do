redo-ifchange ../conf/build_mode ../conf/flags ../conf/libtype ../tools/redo_gprbuild ../lib/xreqlib
. ../conf/build_mode
. ../conf/flags
. ../conf/libtype
. ../tools/redo_gprbuild

GPR_FLAGS="-Xtype=$LIBTYPE -Xmode=$BUILD_MODE"
redo_gprbuild ../xreq.gpr "$3"

