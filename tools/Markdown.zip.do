eval $(../redoconf/sh-init)
rc_source ./config
wget "$MARKDOWN_URL" -O "$3" >&2
