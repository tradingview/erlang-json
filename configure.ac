dnl Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
dnl Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
dnl
dnl This file is part of EEP0018, which is released under the MIT license.

AC_PREREQ([2.59])

AC_INIT([eep0018], 0.1, [paul.joseph.davis@gmail.com])

AM_INIT_AUTOMAKE(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)

AC_GNU_SOURCE
AC_ENABLE_SHARED
AC_DISABLE_STATIC
AC_CONFIG_MACRO_DIR([m4])
AM_CONFIG_HEADER(config.h)

# Check for programs
AC_PROG_CC
AC_PROG_LIBTOOL
AC_PATH_PROG([ERL], [erl])

if test x${ERL} = x; then
    AC_MSG_ERROR([Could not find the 'erl' executable. Is Erlang installed?])
fi

erlang_version_error="The installed Erlang version is less tahn 5.6.0 (R12B)"
version="`${ERL} -version 2>&1 | ${SED} "s/[[^0-9]]/ /g"`"

if test `echo $version | ${AWK} "{print \\$2}"` -lt 6; then
    AC_MSG_ERROR([$erlang_version_error])
fi

AC_PATH_PROG([ERLC], [erlc])

if test x${ERLC} = x; then
    AC_MSG_ERROR([Could not find the 'erlc' executable. Is Erlang installed?])
fi

# Check for erlang
prog="io:format(\"~s~n\", [[code:root_dir()]]), init:stop()."
ERL_DIR="`erl -noshell -noinput -eval "$prog"`"
ERTS_DIR=`ls -d $ERL_DIR/erts*`
EI_DIR=`ls -d $ERL_DIR/lib/erl_interface*`
ERLANG_CFLAGS="-I$ERTS_DIR/include -I$EI_DIR/include"
ERLANG_LFLAGS="-L$EI_DIR/lib"

AC_ARG_WITH([json-include], [AC_HELP_STRING([--with-json-include=PATH],
    [set PATH to the JSON-C include directory.])], [
    JSON_FLAGS="-I$withval"
], [
    JSON_FLAGS="-I/usr/include"
    JSON_FLAGS="$JSON_FLAGS -I/usr/include/json"
    JSON_FLAGS="$JSON_FLAGS -I/usr/local/include/json"
    JSON_FLAGS="$JSON_FLAGS -I/opt/local/include/json"
])

CPPFLAGS="$ERLANG_CFLAGS"
LIB_FLAGS="$ERLANG_LFLAGS -L/usr/local/lib -L/opt/local/lib"
LDFLAGS="$LIB_FLAGS $LDFLAGS"

# Checks for header files.

AC_CHECK_HEADER([erl_driver.h], [], [
    AC_MSG_ERROR([Could not find the 'erl_driver.h' header.])])

AC_CHECK_HEADER([ei.h], [], [
    AC_MSG_ERROR([Could not find the 'ei.h' header.'])])

# Checks for libraries

AC_CHECK_LIB([ei], [ei_decode_atom], [], [
    AC_MSG_ERROR([Could not find the Erlang interface library.

Is Erlang installed?])])

AC_OUTPUT([Makefile src/Makefile tests/Makefile])

