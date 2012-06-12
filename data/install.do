redo-ifchange ../conf/install_dirs
. ../conf/install_dirs

redo-always

set -x

mkdir -p ${DESTDIR}${DATADIR}/gprconfig
install -m644 gprconfig.xml ${DESTDIR}${DATADIR}/gprconfig/xreq.xml

