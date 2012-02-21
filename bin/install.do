redo-ifchange ../conf/install_dirs ../conf/install_mode xreq
. ../conf/install_mode
. ../conf/install_dirs

redo-always

set -x

mkdir -p ${DESTDIR}${BINDIR}
install -m755 xreq ${DESTDIR}${BINDIR}/xreq

