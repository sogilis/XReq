redo-ifchange "tools/Markdown.pl"

exec >&2

if [ -f "$2.mdwn" ]; then
  tools/Markdown.pl <"$2.mdwn" >"$3"
elif [ -f "$2.md" ]; then
  tools/Markdown.pl <"$2.md" >"$3"
elif [ -f "$2" ]; then
  tools/Markdown.pl <"$2" >"$3"
else
  echo "Cannot make $1"
  exit 1
fi

