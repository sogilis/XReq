redo-always
exec >&2

redo-ifchange verbose_level
. ./verbose_level

if [ 0 == "$VERBOSE" ]; then
  : ${GPRBUILD_VERBOSE_FLAGS:=-q}
  : ${XREQ_FLAGS:=-q}
elif [ 1 == "$VERBOSE" ]; then
  : ${GPRBUILD_VERBOSE_FLAGS:=}
  : ${XREQ_FLAGS:=}
else
  : ${GPRBUILD_VERBOSE_FLAGS:=-v}
  : ${XREQ_FLAGS:=}
fi

: ${GPRBUILD:=gprbuild}
: ${GPRBUILD_FLAGS:=-s -p $GPRBUILD_VERBOSE_FLAGS}

echo
echo "-------------------------- COMPILATION SWITCHES --------------------------"
echo "--"
echo "--  GPRBUILD_FLAGS  $GPRBUILD_FLAGS"
echo "--  XREQ_FLAGS      $XREQ_FLAGS"
echo "--"
echo "--------------------------------------------------------------------------"
echo "--  You can change these either by changing the environment or editing  --"
echo "--  $(pwd)/$2"
echo "--                                                                      --"
echo "--  If you change that file manually, it won't be overwritten. Delete   --"
echo "--  to restore the defaults                                             --"
echo "--------------------------------------------------------------------------"
echo

exec >"$3"

for var in GPRBUILD GPRBUILD_FLAGS XREQ_FLAGS; do
  echo "export $var='$(eval echo \$$var)'"
done

redo-stamp <"$3"
