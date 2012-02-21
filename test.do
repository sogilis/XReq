redo bin/xreq.dbg

export XREQ_LANG=Ada
echo $XREQ_LANG > .cucumber-rerun-lang.txt
cucumber \
  -t "~@wip" -t "~@bootstrap" -t "@lang-$XREQ_LANG,~@lang" \
  -f progress \
  -f rerun -o .cucumber-rerun.txt \
  features/*.feature >&2
