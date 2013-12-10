### ===========================================================================
### File: "configure.ac"
###                        Created: 2010-06-24 08:46:27
###              Last modification: 2013-12-10 19:14:26
### Author: Bernard Desgraupes
### e-mail: <bernard.desgraupes@u-paris10.fr>
### This is part of the R package 'clusterCrit'.
### ===========================================================================
###
### Process this file with autoconf to produce a configure script.
###


AC_PREREQ(2.60)

AC_INIT([clusterCrit],[1.0])

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
   echo "could not determine R_HOME"
   exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
FC=`"${R_HOME}/bin/R" CMD config FC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
FCFLAGS=`"${R_HOME}/bin/R" CMD config FCFLAGS`
FLIBS=`"${R_HOME}/bin/R" CMD config FLIBS`

# C support
# ---------
AC_LANG(C)
AC_PROG_CC(${CC})

# Fortran support
# ---------------
AC_PROG_FC(${FC})



# Process the .in files
# ---------------------
AC_CONFIG_FILES(
[ 
src/Makevars
])

AC_OUTPUT


