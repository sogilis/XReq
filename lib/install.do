redo-ifchange ../conf/install_mode ../conf/install_dirs
. ../conf/install_mode
. ../conf/install_dirs

redo-always
redo-ifchange xreqlib xreqlib.installed.gpr

set -x

mkdir -p ${DESTDIR}${GPRDIR}
cp -R xreqlib.installed.gpr ${DESTDIR}${GPRDIR}/xreqlib.gpr

rm -rf ${DESTDIR}${LIBDIR}/xreqlib
cp -R xreqlib ${DESTDIR}${LIBDIR}

