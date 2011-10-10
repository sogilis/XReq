eval $(../sh-init)
rc_source ./c_compiler.rc

if ! have_c_library "m" "math.h" ; then
	rc_record HAVE_M=""
	exit 0
fi

rc_record HAVE_M="1"
rc_append LDFLAGS "-lm"
