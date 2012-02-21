redo-ifchange ../conf/install-dirs xreq
. ../conf/install-dirs

mkdir -p ${DESTDIR}${BINDIR}
install -m755 xreq.rel ${DESTDIR}${BINDIR}/xreq

