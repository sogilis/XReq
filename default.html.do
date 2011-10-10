eval $(redoconf/sh-init)
rc_source ./config

if [ -f "$1.mdwn" ]; then
  compile_md "$1.mdwn"
elif [ -f "$1.md" ]; then
  compile_md "$1.md"
elif [ -f "$1" ]; then
  compile_md "$1"
fi

