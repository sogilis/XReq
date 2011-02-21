
case "$1" in
  xreqlib-format_html_template.ads)
    redo-ifchange ../xreq-report.template.html ../../tools/template.pl
    perl ../../tools/template.pl ../xreq-report.template.html XReqLib.Format_HTML_Template "$3.ads" tmp.adb >/dev/null
    mv "$3.ads" "$3"
    rm -f tmp.adb
  ;;
  xreqlib-format_html_template.adb)
    redo-ifchange ../xreq-report.template.html ../../tools/template.pl
    perl ../../tools/template.pl ../xreq-report.template.html XReqLib.Format_HTML_Template tmp.ads "$3" >/dev/null
    rm -f tmp.ads
  ;;
esac