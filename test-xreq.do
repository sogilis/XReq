exec >&2
export GNAT_FLAGS="-g -gnata -E"
xreq -q -m -x feature_tests -o features/tests/dbg features/*.feature
./features/tests/dbg/feature_tests

