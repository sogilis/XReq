set -e

redo-ifchange ../obj/main
cp ../obj/main "$3"
