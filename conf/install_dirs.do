#!/bin/bash

redo-ifchange help
redo-always
exec >&2

: ${DESTDIR:=}
: ${PREFIX:=`../tools/whereis gprbuild`}
: ${PREFIX_GPS:=`../tools/whereis gps`}
: ${PREFIX:=/usr/local}

: ${BINDIR:=$PREFIX/bin}
: ${INCLUDEDIR:=$PREFIX/include}
: ${LIBDIR:=$PREFIX/lib}
: ${GPRDIR:=$PREFIX/lib/gnat}
: ${DATADIR:=$PREFIX/share}
: ${DOCDIR:=$DATADIR/doc/XReq}

echo
echo "------------------------ INSTALLATION DIRECTORIES ------------------------"
echo "--"
echo "--  DESTDIR     $DESTDIR"
echo "--  PREFIX      $PREFIX"
echo "--  PREFIX_GPS  $PREFIX_GPS"
echo "--"
echo "--  BINDIR      $BINDIR"
echo "--  INCLUDEDIR  $INCLUDEDIR"
echo "--  LIBDIR      $LIBDIR"
echo "--  GPRDIR      $GPRDIR"
echo "--  DATADIR     $DATADIR"
echo "--  DOCDIR      $DOCDIR"

if [ -n "$PREFIX_GPS" ]; then
  : ${GPSDATADIR:=$PREFIX_GPS/share/gps}
  echo "--"
  echo "--  GPSDATADIR  $GPSDATADIR"
fi

echo "--"
echo "--  Conf file: $(pwd)/$2"
echo "--------------------------------------------------------------------------"

if [ -n "$DESTDIR" ] && [ / != "${DESTDIR:0:1}" ]; then
  echo "ERROR: DESTDIR is not an absolute path." >&2
  exit 1
fi

exec >"$3"

for var in DESTDIR PREFIX PREFIX_GPS BINDIR INCLUDEDIR LIBDIR GPRDIR DATADIR DOCDIR GPSDATADIR; do
  echo "export $var='$(eval echo \$$var)'"
done

redo-stamp <"$3"
