redo-ifchange help
redo-always
exec >&2

: ${BUILD_MODE:=debug}

case $BUILD_MODE in
  debug|dbg)    BUILD_MODE=debug    ;;
  release|rel)  BUILD_MODE=release  ;;
  coverage|cov) BUILD_MODE=coverage ;;
  *)            BUILD_MODE=debug    ;;
esac

echo
echo "------------------------------- BUILD MODE -------------------------------"
echo "--"
echo "--  BUILD_MODE=$BUILD_MODE [debug, dbg, release, rel, coverage, cov]"
echo "--"
echo "--  Conf file: $(pwd)/$2"
echo "--------------------------------------------------------------------------"

echo "export BUILD_MODE=$BUILD_MODE" | tee "$3" | redo-stamp


