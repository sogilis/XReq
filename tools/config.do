exec >"$3"

cat <<"EOF"

MARKDOWN_URL=http://daringfireball.net/projects/downloads/Markdown_1.0.1.zip
MARKDOWN_DIR=Markdown_1.0.1
MARKDOWN_CMDLINE='tools/Markdown.pl <$< >$@'

EOF

