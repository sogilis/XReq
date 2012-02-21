redo-ifchange ../../conf/install_dirs
. ../../conf/install_dirs

redo-always
redo-ifchange sources

set -x

rm -rf ${DESTDIR}${INCLUDEDIR}/xreqlib
mkdir -p ${DESTDIR}${INCLUDEDIR}/xreqlib
cp *.ad[bs] ${DESTDIR}${INCLUDEDIR}/xreqlib

