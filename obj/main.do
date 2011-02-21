set -e

redo-ifchange b~main.o
cat b~main.deps | xargs redo-ifchange
gnatlink -o $3 main.ali >&2
