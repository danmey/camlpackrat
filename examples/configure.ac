AC_INIT([src], [1.2])

AC_CONFIG_AUX_DIR([.])
AM_INIT_AUTOMAKE

AC_PROG_CC

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
  AC_MSG_ERROR([Please install the OCaml compiler])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
  AC_MSG_ERROR([Please install OCaml findlib (the ocamlfind command)])
fi

dnl AC_CHECK_OCAML_PKG([lablgtk2])
dnl if test "$OCAML_PKG_lablgtk2" = "no"; then
dnl   AC_MSG_ERROR([Please install OCaml findlib module 'lablgtk2'.])
dnl fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
