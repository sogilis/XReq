set -e

GNATBINDFLAGS=
SRCDIR=../src

case $1 in

  b~*)
    name="$(cut -c3- <<<"$1")"
    redo-ifchange "$name.o"
    #sed -rn -e 's/^W[^\t]*\t+(\S+).*$/\1/p' -e 's/^D\s+(\S+).*$/\1/p' <"$name.ali" \
    #  | xargs -n 1 find "$SRCDIR" -name \
    #  | xargs -n 1 sh -c 'echo "$(basename "$0" | sed s/ad\[bs\]\$/o/)"' \
    #  | xargs redo-ifchange
    find ../src -name "*.ad[bs]" -and '!' -path "*/gps/*" \
      | xargs -n 1 basename \
      | cut -d. -f1 \
      | sort | uniq \
      | xargs -n 1 printf '%s.o\n' \
      | tee $1.deps \
      | xargs redo-ifchange
    echo "$1.o" >> "$1.deps"
    mkdir -p tmp
    gnatbind -o "tmp/$1.adb" "$name.ali" -I- -aO. $GNATBINDFLAGS
    mv "tmp/$1.adb" "$3"
    mv "tmp/$1.ads" "$1.ads"
    sed -i 's:tmp/b~:b~:' "$3" "$1.ads"
    rmdir tmp 2>/dev/null || true
    ;;
  *)
    echo "No rule to make $1$2" >&2
    exit 1
    ;;

esac

