eval $(../redoconf/sh-init)

redo-always

rc_source ../config
GPRBUILD_FLAGS+="-Xmode=debug"

compile_gpr ../xreq.gpr
mv xreq.dbg "$3"

