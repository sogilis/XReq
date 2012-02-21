redo-ifchange help
redo-always
exec >&2

: ${VERBOSE:=1}

echo
echo "---------------------------- BUILD VERBOSITY  ----------------------------"
echo "--"
echo "--  VERBOSE=$VERBOSE [0, 1, 2]"
echo "--"
echo "--  Conf file: $(pwd)/$2"
echo "--------------------------------------------------------------------------"

echo "export VERBOSE=$VERBOSE" | tee "$3" | redo-stamp

