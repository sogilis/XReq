eval $(../sh-init)

if [ -z "$BUILDPLATFORM" ]; then
	if which uname >/dev/null; then
		BUILDPLATFORM=$(uname)

		if [ "$BUILDPLATFORM" = "Darwin" ]; then
			BUILDPLATFORM="MacOSX"
		fi
	else
		if [ -n "$OS" ]; then
			BUILDPLATFORM=$OS
		else
			die "Unsupported platform."
		fi
	fi
fi

rc_record BUILDPLATFORM
