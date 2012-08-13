#!/usr/bin/env bash
redo-ifchange conf/install_dirs
. conf/install_dirs

redo-always

set -x

if [ -n "$GPSDATADIR" ]; then
  mkdir -p ${DESTDIR}${GPSDATADIR}/plug-ins
  mkdir -p ${DESTDIR}${GPSDATADIR}/icons/24px/
	install -m644 data/gps-plug-in/feature-lang.xml ${DESTDIR}${GPSDATADIR}/plug-ins/feature-lang.xml
	install -m644 data/gps-plug-in/xreq-redo.xml ${DESTDIR}${GPSDATADIR}/plug-ins/xreq-redo.xml
	sed -i "s/%GPSDATADIR%/${GPSDATADIR//\//\\/}/" ${DESTDIR}${GPSDATADIR}/plug-ins/xreq-redo.xml
	install -m644 data/gps-plug-in/xreq-redo.png ${DESTDIR}${GPSDATADIR}/icons/24px/xreq-redo.png
fi


