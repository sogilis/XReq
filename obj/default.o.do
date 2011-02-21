set -e

# ADAFLAGS: flags for gcc
# SRCDIR:   where the source files can be found

SRCDIR=../src
ADAFLAGS="-I../src -I../src/lib -I../src/common -I../src/lib -I../src/lib/static -gnat05"

# Depend on source file
adb="$( find "$SRCDIR" -name "$1.ad[bs]" | sort | head -n 1)"
[ -z "$adb" ] && adb="$1.adb"
redo-ifchange "$adb"

# Generate in a tempoary directory since gnat checks that the object file has
# the name of the unit followed by the .o suffix exactly. No leeway is allowed
mkdir -p tmp

# Compile
gcc $ADAFLAGS -c -o "tmp/$1.o" "$adb"

# Tell redo of the dependencies. We find this information in the .ali file
test -f "tmp/$1.ali"
sed -rn 's/^W[^\t]*\t+(\S+).*$/\1/p' <"tmp/$1.ali" \
  | xargs -n 1 find "$SRCDIR" -name \
  | xargs redo-ifchange

# Move the object file to $3 and the ali file to what it should be then cleanup
mv "tmp/$1.o"   "$3"
mv "tmp/$1.ali" "$1.ali"
rmdir tmp 2>/dev/null || true

exit 0
