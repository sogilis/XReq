redo-ifchange ../conf/build_mode ../conf/flags ../conf/libtype ../tools/redo_gprbuild ../src/lib/sources
. ../conf/build_mode
. ../conf/flags
. ../conf/libtype
. ../tools/redo_gprbuild

redo-always

GPR_FLAGS="-Xtype=$LIBTYPE -Xmode=$BUILD_MODE"
redo_gprbuild ../xreq.gpr xreqlib.tmp

