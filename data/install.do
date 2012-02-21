redo-ifchange ../conf/install_dirs
. ../conf/install_dirs

redo-always

set -x

if [ -n "$GPSDATADIR" ]; then
  mkdir -p ${DESTDIR}${GPSDATADIR}/plug-ins
	install -m644 gps-plug-in/feature-lang.xml ${DESTDIR}${GPSDATADIR}/plug-ins/feature-lang.xml
fi

mkdir -p ${DESTDIR}${DATADIR}/gprconfig
install -m644 gprconfig.xml ${DESTDIR}${DATADIR}/gprconfig/xreq.xml

