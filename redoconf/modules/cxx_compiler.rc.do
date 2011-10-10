eval $(../sh-init)

POTENTIAL="g++ c++"

_check_cxx_compiler () {
	local CXX=$1
	local CXXFLAGS=$2
	local RESULT=1

	# Make a simple C++ file we can test-compile.
	TEMPSOURCE=temp.cpp
	TEMPBINARY=temp.o
	rm -rf "$TEMPSOURCE" "$TEMPBINARY"

	cat >"$TEMPSOURCE" <<- EOF
	#include <iostream>

	int main(int argc, char *argv[])
	{
		return 0;
	}
	EOF

	log "Checking to see if $CXX is a sane C++ compiler..."

	# Try compiling 
	if $CXX $CXXFLAGS -c -o "$TEMPBINARY" "$TEMPSOURCE" 1>&2; then
		RESULT=0
	fi

	rm -rf "$TEMPSOURCE" "$TEMPBINARY"

	return $RESULT
}

if [ -n "$CXX" ]; then
	rc_record HAVE_CXX_COMPILER="1" CXX CXXFLAGS
else
	HAVE_CXX_COMPILER=""
	for CXX in $POTENTIAL; do
		if _check_cxx_compiler "$CXX" "$CXXFLAGS"; then
			HAVE_CXX_COMPILER="1"
			break
		fi
	done

	rc_record HAVE_CXX_COMPILER CXX CXXFLAGS
fi

cat << "EOF"
cxx_compile () {
	local DEPS

	if [ -z "$HAVE_CXX_COMPILER" ]; then
		return 1
	fi

	local TARGET="$1"; shift
	# Generate the dependency information, in case we need to build
	# anything before we actually do the compilation.
	local depsfile="$1.deps"
	rm -rf "$depsfile"
	$CXX $CPPFLAGS $CXXFLAGS -M -MF "$depsfile" -MG "$@" 1>&2
	read DEPS < "$depsfile"
	redo-ifchange ${DEPS:#*:}
	rm "$depsfile"

	# Now our dependencies are met, do the compilation.
	$CXX $CPPFLAGS $CXXFLAGS -c -o "$TARGET" "$@" 1>&2
}

cxx_link () {
	if [ -z "$HAVE_CXX_COMPILER" ]; then
		return 1
	fi

	local TARGET="$1"; shift
	redo-ifchange "$@"

	$CXX -o "$TARGET" "$@" $LDFLAGS 1>&2
}

have_cxx_library () {
	if [ -z "$HAVE_CXX_COMPILER" ]; then
		return 1
	fi

	local libname="$1"
	local headername="$2"
	local testldflags="$LDFLAGS"
	local res=1

	local sourcefile="redoconf_temp.$$.cpp"
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
	if $CXX $CPPFLAGS $CXXFLAGS $testldflags -o "$binary" "$sourcefile" 1>&2 ; then
		# Everything went well, we can use this library
		res=0
	fi

	rm -f "$sourcefile" "$binary"

	return "$res"
}
EOF
