eval $(../redoconf/sh-init)

rm -f *.aunit.xml
rm -f gnatcheck*.out
rm -f gnatcheck*.log
rm -f *.gcov
rm -f features*.html
rm -f features*.junit/*

rc_clean

