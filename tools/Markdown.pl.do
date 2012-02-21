redo-ifchange config Markdown.zip
. ./config

unzip -qq -c Markdown.zip "$MARKDOWN_DIR/Markdown.pl" >"$3"
chmod +x "$3"

