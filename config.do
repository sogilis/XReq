eval $(redoconf/sh-init)

rc_record TOPDIR="$(pwd)"

cat <<"EOF"

eval $("$TOPDIR/redoconf/sh-init")

# dynamic makes gcov unhappy: hidden symbol `__gcov_merge_add' is referenced by
# DSO (Dynamic Shared Object).
LIBTYPE=static

GPRBUILD_FLAGS="-p -Xtype=$LIBTYPE"
XREQ_FLAGS=

if [ -z "$VERBOSE" ]; then
  GPRBUILD_FLAGS+=-q
  XREQ_FLAGS=-q
elif [ "$VERBOSE" -gt 0 ]; then
  GPRBUILD_FLAGS+="-v"
fi

compile_gpr(){
  redo-ifchange "$1"
  gprbuild -P"$1" >&2
}

compile_md(){
  redo-ifchange "$TOPDIR/tools/Markdown.pl"
  "$TOPDIR/tools/Markdown.pl" < "$1"
}


EOF

