eval $(redoconf/sh-init)

redo bin/xreq.dbg
make lib >&2

files="$(
find features/data -maxdepth 1 -name "*.feature" -o -name "*.requirement" \
  | egrep -v 'ambiguous|empty|comments|error')"

xargs redo-ifchange <<<"$files"

xargs bin/xreq.dbg -m -s features/data/step_definitions -s features/data/step_definitions2 -x example_suite <<<"$files" >&2

set +e

features/data/tests/example_suite -f HTML -o "$3" -f text >&2
true

