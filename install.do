redo-always
redo data/install bin/install lib/install src/lib/install
redo-ifchange conf/install_dirs

. conf/install_dirs

exec >&2

echo "--------------------------------------------------------------------------"
echo "--                        INSTALLATION  FINISHED                        --"
echo "--------------------------------------------------------------------------"
echo "-- XReq has now been installed in:                                      --"
echo "-- $DESTDIR$PREFIX"

if [ -n "$DESTDIR" ]; then
  echo "--                                                                      --"
  echo "-- Note: you set DESTDIR to a non empty value, the installation is not  --"
  echo "-- finished yet. You must make sure DESTDIR is packaged (using the fpm  --"
  echo "-- ruby gem for example) or chroot in DESTDIR before use.               --"
  echo "--                                                                      --"
  echo "-- DESTDIR=$DESTDIR"
fi
echo "--                                                                      --"
echo "-- To be able to use the xreq binary, you may need to update your PATH  --"
echo "-- to point to:                                                         --"
echo "-- BINDIR=$BINDIR"
echo "--                                                                      --"
echo "-- To be able to use the library for Ada, you may need to update your   --"
echo "-- ADA_PROJECT_PATH or GPR_PROJECT_PATH to point to:                    --"
echo "-- GPRDIR=$GPRDIR"
echo "--                                                                      --"
echo "-- To be able to use the library, you may need to update your           --"
echo "-- LD_LIBRARY_PATH to point to:                                         --"
echo "-- LIBDIR=$LIBDIR"
echo "--------------------------------------------------------------------------"

