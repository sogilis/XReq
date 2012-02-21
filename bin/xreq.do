redo-ifchange ../conf/build_mode ../conf/flags ../conf/libtype
. ../conf/build_mode
. ../conf/flags
. ../conf/libtype

redo-always
exec >&2

$GPRBUILD $GPRBUILD_FLAGS -P../xreq.gpr -Xtype=$LIBTYPE -Xmode=$BUILD_MODE -o "$3"

