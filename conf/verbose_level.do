redo-always
exec >&2

: ${VERBOSE:=1}

echo
echo "--------------------------------------------------------------------------"
echo "--  Your current verbosity level: VERBOSE=$VERBOSE [0, 1, 2]"
echo "--------------------------------------------------------------------------"
echo "--  You can change this either by changing the environment variable     --"
echo "--  VERBOSE or editing                                                  --"
echo "--  $(pwd)/$2"
echo "--                                                                      --"
echo "--  If you change that file manually, it won't be overwritten. Delete   --"
echo "--  to restore the defaults                                             --"
echo "--------------------------------------------------------------------------"
echo

echo "export VERBOSE=$VERBOSE" | tee "$3" | redo-stamp

