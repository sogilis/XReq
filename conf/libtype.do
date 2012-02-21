redo-always
exec >&2

: ${LIBTYPE:=dynamic}

case $LIBTYPE in
  static|dynamic) ;;
  *) LIBTYPE=dynamic ;;
esac

# Do some magic detection to know if dynamic libraries are not supported

echo
echo "--------------------------------------------------------------------------"
echo "--  Your current library type: LIBTYPE=$LIBTYPE [static, dynamic]"
echo "--------------------------------------------------------------------------"
echo "--  You can change this either by changing the environment variable     --"
echo "--  LIBTYPE or editing                                                  --"
echo "--  $(pwd)/$2"
echo "--                                                                      --"
echo "--  If you change that file manually, it won't be overwritten. Delete   --"
echo "--  to restore the defaults                                             --"
echo "--------------------------------------------------------------------------"
echo

echo "export LIBTYPE=$LIBTYPE" | tee "$3" | redo-stamp

