AC_INIT(test, 1.0)

AC_CONFIG_COMMANDS([abc],
                   [echo "Testing $mypkgname"],
                   [mypkgname=$PACKAGE_NAME])
AC_CONFIG_COMMANDS([def],
		   [echo "DEF"])

AC_CONFIG_HEADERS([config.h])
AC_CHECK_HEADERS([unistd.h foobar.h baz.h])

AC_OUTPUT
