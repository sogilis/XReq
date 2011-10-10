eval $(../sh-init)

POTENTIAL="gcc cc"

_check_c_compiler () {
	local CC=$1
	local CFLAGS=$2
	local RESULT=1

	# Make a simple C file we can test-compile.
	TEMPSOURCE=temp.c
	TEMPBINARY=temp.o
	rm -rf "$TEMPSOURCE" "$TEMPBINARY"

	cat >"$TEMPSOURCE" <<- EOF
	#include <stdio.h>

	int main(int argc, char *argv[])
	{
		return 0;
	}
	EOF

	log "Checking to see if $CC is a sane C compiler..."

	# Try compiling 
	if $CC $CFLAGS -c -o "$TEMPBINARY" "$TEMPSOURCE" 1>&2; then
		RESULT=0
	fi

	rm -rf "$TEMPSOURCE" "$TEMPBINARY"

	return $RESULT
}

if [ -n "$CC" ]; then
	rc_record HAVE_C_COMPILER="1" CC CFLAGS
else
	HAVE_C_COMPILER=""
	for CC in $POTENTIAL; do
		if _check_c_compiler "$CC" "$CFLAGS"; then
			HAVE_C_COMPILER="1"
			break
		fi
	done

	rc_record HAVE_C_COMPILER CC CFLAGS
fi

cat << "EOF"
c_compile () {
	local DEPS

	if [ -z "$HAVE_C_COMPILER" ]; then
		return 1
	fi

	local TARGET="$1"; shift
	# Generate the dependency information, in case we need to build
	# anything before we actually do the compilation.
	local depsfile="$1.deps"
	rm -rf "$depsfile"
	$CC $CPPFLAGS $CFLAGS -M -MF "$depsfile" -MG "$@" 1>&2
	read DEPS < "$depsfile"
	redo-ifchange ${DEPS#*:}
	rm "$depsfile"

	# Now our dependencies are met, do the compilation.
	$CC $CPPFLAGS $CFLAGS -c -o "$TARGET" "$@" 1>&2
}

c_link () {
	if [ -z "$HAVE_C_COMPILER" ]; then
		return 1
	fi

	local TARGET="$1"; shift
	redo-ifchange "$@"

	$CC -o "$TARGET" "$@" $LDFLAGS 1>&2
}

have_c_library () {
	if [ -z "$HAVE_C_COMPILER" ]; then
		return 1
	fi

	local libname="$1"
	local headername="$2"
	local testldflags="$LDFLAGS"
	local res=1

	local sourcefile="redoconf_temp.$$.c"
	local binary="redoconf_temp.$$.out"

	# Make a C file that tries to include the mentioned header file.
	if [ -n "$headername" ]; then
		echo "#include <$headername>" > "$sourcefile"
	fi
	echo "int main(int argc, char *argv[]) { return 0; }" >> "$sourcefile"

	if [ -n "$libname" ]; then
		testldflags="$testldflags -l$libname"
	fi

	# Try compiling our tempfile and linking against the requested library.
	if $CC $CPPFLAGS $CFLAGS $testldflags -o "$binary" "$sourcefile" 1>&2 ; then
		# Everything went well, we can use this library
		res=0
	fi

	rm -f "$sourcefile" "$binary"

	return "$res"
}
EOF
