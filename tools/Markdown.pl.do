eval $(../redoconf/sh-init)
redo-ifchange Markdown.zip
rc_source ./config

unzip -qq -c Markdown.zip "$MARKDOWN_DIR/Markdown.pl" >"$3"
chmod +x "$3"

