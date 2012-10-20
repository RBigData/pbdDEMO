dnl Process this file with autoconf to produce a configure script
AC_INIT(DESCRIPTION)

SPMD=`"${R_HOME}/bin/Rscript" -e "cat(system.file(package='pbdMPI'))"`
if test "X$SPMD" = "X" ; then
  AC_MSG_ERROR([pbdMPI is not installed.])
fi

SLAP=`"${R_HOME}/bin/Rscript" -e "cat(system.file(package='pbdSLAP'))"`
if test "X$SLAP" = "X" ; then
  AC_MSG_ERROR([pbdSLAP is not installed.])
fi

dnl Start to substitute templates
AC_OUTPUT(src/Makevars)
