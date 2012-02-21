redo-ifchange help
redo-always
exec >&2

: ${LIBTYPE:=dynamic}

case $LIBTYPE in
  static|dynamic) ;;
  *) LIBTYPE=dynamic ;;
esac

echo
echo "------------------------------ LIBRARY TYPE ------------------------------"
echo "--"
echo "--  LIBTYPE=$LIBTYPE [static, dynamic]"
echo "--"
echo "--  Conf file: $(pwd)/$2"
echo "--------------------------------------------------------------------------"

echo "export LIBTYPE=$LIBTYPE" | tee "$3" | redo-stamp

