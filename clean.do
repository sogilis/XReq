eval $(redoconf/sh-init)

set +e

(

make cov-clean

gprclean -q -Pxreq.gpr    -Xtype=dynamic -Xmode=debug
gprclean -q -Pxreq.gpr    -Xtype=static  -Xmode=release
gprclean -q -Pxreq.gpr    -Xtype=dynamic -Xmode=coverage
gprclean -q -Pxreq.gpr    -Xtype=static  -Xmode=debug
gprclean -q -Pxreq.gpr    -Xtype=dynamic -Xmode=release
gprclean -q -Pxreq.gpr    -Xtype=static  -Xmode=coverage
gprclean -q -Plibxreq.gpr -Xtype=dynamic -Xmode=debug
gprclean -q -Plibxreq.gpr -Xtype=dynamic -Xmode=coverage
gprclean -q -Plibxreq.gpr -Xtype=dynamic -Xmode=release
gprclean -q -Pxreqlib.gpr -Xtype=dynamic -Xmode=debug
gprclean -q -Pxreqlib.gpr -Xtype=static  -Xmode=release
gprclean -q -Pxreqlib.gpr -Xtype=dynamic -Xmode=coverage
gprclean -q -Pxreqlib.gpr -Xtype=static  -Xmode=debug
gprclean -q -Pxreqlib.gpr -Xtype=dynamic -Xmode=release
gprclean -q -Pxreqlib.gpr -Xtype=static  -Xmode=coverage

rm -rf tmp
rm -rf obj/*/*
rm -rf lib/*/*
rm bin/*
rm README.html
rm src/README.html
rm reports/*.aunit.xml
rm reports/gnatcheck*.out
rm reports/gnatcheck*.log
rm reports/*.gcov
rm reports/features*.html
rm reports/features*.junit/*
rm features/data/tmp-*
rm features/data/step_definitions*/*.[od]
rm features/data/step_definitions*/*.gcda
rm features/data/step_definitions*/*.gcno
rm -rf features/tests/*
rm -rf tests/features/tests/*

find . -name "*~" -print0 | xargs -0 rm

) 2>/dev/null >&2


