# Utility functions generally useful for shell programming.

# Display the given text on stderr, so it's seen by the user.
log () {
	printf "%s\n" "$*" >&2
}

# Display the given text on stderr and return a failure code.
die () {
	log "$@"
	exit 1
}

# Prints a shell-quoted version of the parameter.
rc_shquote () {
	local HEAD TAIL="$*"
	printf "'"

	while [ -n "$TAIL" ]; do
		HEAD="${TAIL%%\'*}"

		if [ "$HEAD" = "$TAIL" ]; then
			printf "%s" "$TAIL"
			break
		fi

		printf "%s'\"'\"'" "$HEAD"

		TAIL="${TAIL#*\'}"
	done

	printf "'"
}

# Prints an absolutised version of the given path.
rc_abspath () {
	local DIRNAME=$(dirname "$1")
	local BASENAME=$(basename "$1")

	if [ -d "$DIRNAME" ]; then
		( cd "$DIRNAME"; printf "%s" "$PWD" )
	else
		rc_abspath "$DIRNAME"
	fi

	printf "/%s" "$BASENAME"
}


# Rebuild the given file and source it into the current shell.
rc_source () {
	redo-ifchange "$@"

	while [ $# -gt 0 ]; do
		. "$1"
		shift
	done
}

# Rebuild the given file and print it to stdout.
rc_cat () {
	redo-ifchange "$@"
	cat "$@"
}

# Rebuild the given file, source it and print it to stdout.
rc_source_cat () {
	rc_source "$@" && rc_cat "$@"
}

# Rebuild the given file, then print it after removing comments and blanks.
rc_filter_comments () {
	redo-ifchange $1
	sed -e 's/#.*//' "$1" | grep -v "^[[:space:]]*$"
}

# Prints the source(s) listed for a target in a mapping file.
#
# $1 is the filename of the mapping file.
# $2 is the key to look up in the mapping file.
rc_map_get () {
	local MAPFILE=$1
	local KEY=$2
	local VALUE=""

	 rc_filter_comments "$MAPFILE" |
		while read CURR_KEY VALUE; do
			if [ "$KEY" = "$CURR_KEY" ]; then
				printf "%s" "$VALUE"
			fi
		done
}

# Prints the targets listed in a mapping file.
#
# $1 is the filename of the mapping file.
rc_map_keys () {
	rc_filter_comments $1 | cut -d" " -f1
}

# Returns true if the given command is available.
#
# "$1" is the command name to check.
rc_have_command ()
{
	if which "$1" >/dev/null ; then
		# It's an external command, and we have it.
		return 0
	fi

	# Try running the command with as few side-effects as possible.
	local EXIT=$("$1" --help >/dev/null 2>/dev/null || echo "$?")
	if [ -n "$EXIT" ] && [ "$EXIT" -eq 127 ]; then
		# Not found
		return 1
	fi

	return 0
}

# Runs the given command if it is available.
#
# If the command was found, returns the exit code of the command. If the
# command was not found, returns true.
rc_if_exists ()
{
	local CMD=$1
	shift
	if rc_have_command "$CMD"; then
		"$CMD" "$@"
		return $?
	else
		return 0
	fi
}

# Returns true if $1 starts with $2
rc_startswith ()
{
	local strip=${1#$2}
	if [ "$strip" = "$1" ]; then
		return 1  # not found
	else
		return 0  # found
	fi
}

# Remove files created by redo, as best we can.
#
# This function can break the execution of minimal/do, so make sure it's the
# last command run in your clean.do.
rc_clean_targets ()
{
	# Clean up after proper redo.
	rc_if_exists redo-targets | while read x; do
		# ignore anything not in the current directory
		rc_startswith "$x" "./" || echo "$x"
	done | xargs rm -f

	# Clean up after minimal-do
	[ -z "$DO_BUILT" ] || rm -rf .do_built .do_built.dir
}

# Call "clean.do" files in subdirectories.
rc_clean_children ()
{
	for d in */clean.do; do
		[ -e "$d" ] && echo "${d%.do}"
	done | xargs redo
}

# Call the other rc_clean_* functions.
#
# This function may change over time as new rc_clean_* functions are added.
rc_clean ()
{
	rc_clean_children
	rc_clean_targets
}

# Prints a shell command appending the value $2 to the shell variable $1.
#
# Equivalent to calling rc_append_with_delim with the delimiter set to ' '.
rc_append () { rc_append_with_delim "$1" ' ' "$2"; }

# Prints shell commands appending delimiter $2 and value $3 to the variable $1.
#
# If variable $1 is empty, the delimiter is ignored.
rc_append_with_delim () {
	local NAME="$1"
	local DELIM="$(rc_shquote "$2")"
	local VALUE="$(rc_shquote "$3")"

	# Note we use spaces for indenting inside the heredoc, rather than
	# tabs, so that the indenting won't be completely lost in the output
	# file.
	cat <<- EOF
	if [ -z "\$$NAME" ]; then
	        export $NAME=$VALUE
	else
	        export $NAME="\$$NAME"$DELIM$VALUE
	fi
	EOF
}

# Prints a shell command that recreates the state of environment variables.
#
# If you wish to record the state of a variable named "FOO", pass this function
# the string "FOO", not "$FOO".
#
# If you wish to record a variable named FOO as having the contents BAR rather
# than whatever's in the environment, pass this function the string "FOO=BAR"
#
# You can pass multiple variable names:
#
#	rc_record CC CFLAGS CPPFLAGS=-DDEBUG
#
# For technical reasons, rc_record() cannot reliably record the values of
# variables named _RC_RECORD_NAME or _RC_RECORD_VALUE.
rc_record () {
	local _RC_RECORD_NAME
	local _RC_RECORD_VALUE

	while [ $# -gt 0 ]; do
		case "$1" in
			*=*)
				# An explicit name and value.
				_RC_RECORD_NAME=${1%%=*}
				_RC_RECORD_VALUE=${1#*=}
				;;

			*)
				# Read the value from the current environment.
				_RC_RECORD_NAME=$1
				_RC_RECORD_VALUE=$(eval printf "%s" '${'"$1"'}')
				;;
		esac

		if [ -z "$_RC_RECORD_VALUE" ]; then
			# The only tool I know of that distinguishes between
			# unset and null environment variables is pkg-config,
			# and null environment variables make it do stupid
			# things. Therefore, we always unset.
			printf "unset %s\n" "$_RC_RECORD_NAME"
		else
			printf "export %s=" "$_RC_RECORD_NAME"
			rc_shquote "$_RC_RECORD_VALUE"
			printf "\n"
		fi

		shift
	done
}
