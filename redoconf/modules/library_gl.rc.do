eval $(../sh-init)
rc_source ./platform.rc ./c_compiler.rc

if [ "$BUILDPLATFORM" = "Windows_NT" ]; then
	GL_LIB="opengl32"
else
	GL_LIB="GL"
fi

if ! have_c_library "$GL_LIB" "GL/gl.h" ; then
	rc_record HAVE_GL=""
	exit 0
fi

rc_record HAVE_GL=1
rc_append LDFLAGS "-l$GL_LIB"
