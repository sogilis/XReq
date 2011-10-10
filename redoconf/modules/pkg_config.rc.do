eval $(../sh-init)

if [ -n "$PKG_CONFIG" ]; then
	rc_record HAVE_PKG_CONFIG="1" PKG_CONFIG

elif rc_have_command "pkg-config"; then
	rc_record HAVE_PKG_CONFIG="1" PKG_CONFIG="pkg-config"

else
	rc_record HAVE_PKG_CONFIG=""

	# We don't have pkg-config installed, and the answer to this
	# query will probably change when it is installed, so tell redo
	# to keep an eye out.
	(
		IFS=":"

		for p in $PATH; do
			redo-ifcreate $p/pkg-config
		done
	)
	exit 0
fi

rc_record PKG_CONFIG_PATH PKG_CONFIG_LIBDIR PKG_CONFIG_SYSROOT_DIR

cat << "EOF"
# Returns true if pkg-config recognises the given package.
#
# $1 is the package name to check for, other parameters are passed to
# pkg-config so you can use "--atleast-version" and other useful options.
have_pkg () {
	local PACKAGE="$1"; shift

	if [ -z "$PKG_CONFIG" ]; then
		return 1
	fi

	if $PKG_CONFIG --exists "$PACKAGE" "$@"; then
		return 0
	fi

	return 1
}

# Writes shell commands to add flags from the given pkg-config package.
add_pkg () {
	if [ -z "$PKG_CONFIG" ]; then
		return 1
	fi

	local CFLAGS="$($PKG_CONFIG "$1" --cflags)"
	local LDFLAGS="$($PKG_CONFIG "$1" --libs)"

	rc_append CFLAGS   "$CFLAGS"
	rc_append CXXFLAGS "$CFLAGS"
	rc_append LDFLAGS  "$LDFLAGS"
}

# A standard combination of have_pkg and add_pkg
#
# $1 is the name of the package to check for.
#
# $2 is the name of the variable that will be set to "1" or "" to mark
# whether the package was detected.
detect_pkg () {
	local PACKAGE="$1"
	local DEFINE="$2"
	if ! have_pkg "$PACKAGE"; then
		rc_record "$DEFINE"=""
		return
	fi

	rc_record "$DEFINE"="1"
	add_pkg "$PACKAGE"
}
EOF
