eval $(../sh-init)
rc_source ./pkg_config.rc

if ! ( have_pkg "QtCore" && have_pkg "QtGui" ); then
	rc_record HAVE_QT=""
	exit
fi

rc_record HAVE_QT="1"
add_pkg "QtCore"
add_pkg "QtGui"
