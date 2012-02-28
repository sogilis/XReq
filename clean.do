set +e
exec 2>/dev/null >&2

redo bin/clean

make cov-clean

rm -rf tmp
rm -rf obj/*/*
rm -rf lib/*/*
rm README.html
rm src/README.html
rm features/data/tmp-*
rm features/data/step_definitions*/*.[od]
rm features/data/step_definitions*/*.gcda
rm features/data/step_definitions*/*.gcno
rm -rf features/tests/*
rm -rf tests/features/tests/*

find . -name "*~" -print0 | xargs -0 rm

