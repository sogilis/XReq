#!/bin/bash

redo-ifchange ../conf/install_dirs  ../conf/libtype ../conf/build_mode
. ../conf/install_dirs
. ../conf/libtype
. ../conf/build_mode

redo-ifchange build xreqlib.installed.gpr

mkdir -p ${DESTDIR}${GPRDIR}
cp -R xreqlib.installed.gpr ${DESTDIR}${GPRDIR}/xreqlib.gpr

# TODO
rm -rf ${DESTDIR}${INCLUDEDIR}/xreqlib
mkdir -p ${DESTDIR}${INCLUDEDIR}/xreqlib
FILES=(../src/common/*.ad[bs] ../src/lib/*.ad[bs] ../src/lib/$LIBTYPE/*.ad[bs])
redo-ifchange $FILES
cp -R $FILES ${DESTDIR}${INCLUDEDIR}/xreqlib

rm -rf ${DESTDIR}${LIBDIR}/xreqlib
mkdir -p ${DESTDIR}${LIBDIR}/xreqlib
cp lib-$LIBTYPE-$BUILD_MODE/* ${DESTDIR}${LIBDIR}/xreqlib

