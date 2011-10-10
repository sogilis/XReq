eval $(redoconf/sh-init)

rc_record TOPDIR="$(pwd)"

cat <<"EOF"

eval $("$TOPDIR/redoconf/sh-init")

compile_md(){
  redo-ifchange "$TOPDIR/tools/Markdown.pl"
  "$TOPDIR/tools/Markdown.pl" < "$1"
}


EOF

