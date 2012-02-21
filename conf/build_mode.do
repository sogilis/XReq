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
echo "--------------------------------------------------------------------------"
echo "--  Your current build mode: BUILD_MODE=$BUILD_MODE"
echo "--  Accepted values: debug, dbg, release, rel, coverage, cov"
echo "--------------------------------------------------------------------------"
echo "--  You can change this either by changing the environment variable     --"
echo "--  BUILD_MODE or editing                                               --"
echo "--  $(pwd)/$2"
echo "--                                                                      --"
echo "--  If you change that file manually, it won't be overwritten. Delete   --"
echo "--  to restore the defaults                                             --"
echo "--------------------------------------------------------------------------"
echo

echo "export BUILD_MODE=$BUILD_MODE" | tee "$3" | redo-stamp

