dnl Copyright © 2020 Roel Janssen <roel@gnu.org>
dnl
dnl This program is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program. If not, see <http://www.gnu.org/licenses/>.
dnl -*- Autoconf -*-

AC_INIT([pcawg-analysis], [0.0.9], [roel@gnu.org])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([foreign])
AM_SILENT_RULES([yes])

LT_INIT([dlopen win32-dll])
AC_SUBST([LIBTOOL_DEPS])

AC_PROG_CC
AM_PROG_CC_C_O
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h stdio.h])
AC_CHECK_LIB(guile, scm_with_guile)

GUILE_PKG([3.0])
GUILE_PROGS
GUILE_SITE_DIR

dnl Installation directories for .scm and .go files.
moddir="${prefix}/share/guile/site/$GUILE_EFFECTIVE_VERSION"
godir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
extdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/extensions"
AC_SUBST([moddir])
AC_SUBST([godir])
AC_SUBST([extdir])

if $GUILE_TOOLS | grep -q compile; then
   true
else
   AC_MSG_ERROR([Guile 3.0, 2.2, or 2.0 required.])
fi

dnl This macro is used to resolve the absolute path to @datarootdir@.
dnl ---------------------------------------------------------------------------
AC_DEFUN([AC_DEFINE_DIR], [
  prefix_NONE=
  exec_prefix_NONE=
  test "x$prefix" = xNONE && prefix_NONE=yes && prefix=$ac_default_prefix
  test "x$exec_prefix" = xNONE && exec_prefix_NONE=yes && exec_prefix=$prefix
dnl In Autoconf 2.60, ${datadir} refers to ${datarootdir}, which in turn
dnl refers to ${prefix}.  Thus we have to use `eval' twice.
  eval ac_define_dir="\"[$]$2\""
  eval ac_define_dir="\"$ac_define_dir\""
  AC_SUBST($1, "$ac_define_dir")
  AC_DEFINE_UNQUOTED($1, "$ac_define_dir", [$3])
  test "$prefix_NONE" && prefix=NONE
  test "$exec_prefix_NONE" && exec_prefix=NONE
])

dnl Also resolve the absolute paths to the Guile moddir and godir.
dnl ---------------------------------------------------------------------------
AC_DEFINE_DIR([MODDIR], [moddir], [Path to installed Guile modules.])
AC_DEFINE_DIR([GODIR], [godir], [Path to installed Guile objects.])
AC_DEFINE_DIR([EXTDIR], [extdir], [Path to installed Guile extensions.])

dnl Test whether the "guile" command is somewhere in path.
dnl ---------------------------------------------------------------------------
if test "x$GUILE" = "x"; then
   AC_MSG_ERROR([Cannot find GNU Guile])
fi

dnl Test whether the "score-client" command is somewhere in path.
dnl ---------------------------------------------------------------------------
AC_PATH_PROG([SCORECLIENT], [score-client])
if test "x$SCORECLIENT" = "x"; then
   AC_MSG_ERROR([Cannot find score-client.])
fi

dnl Test whether the "samtools" command is somewhere in path.
dnl ---------------------------------------------------------------------------
AC_PATH_PROG([SAMTOOLS], [samtools])
if test "x$SAMTOOLS" = "x"; then
   AC_MSG_ERROR([Cannot find samtools.])
fi

dnl Test whether the "sambamba" command is somewhere in path.
dnl ---------------------------------------------------------------------------
AC_PATH_PROG([SAMBAMBA], [sambamba])
if test "x$SAMBAMBA" = "x"; then
   AC_MSG_ERROR([Cannot find sambamba.])
fi

dnl Test whether the "curl" command is somewhere in path.
dnl ---------------------------------------------------------------------------
AC_PATH_PROG([CURL], [curl])
if test "x$CURL" = "x"; then
   AC_MSG_ERROR([Cannot find curl.])
fi

dnl Test whether the "gsutil" command is somewhere in path.
dnl ---------------------------------------------------------------------------
AC_PATH_PROG([GSUTIL], [gsutil])
if test "x$GSUTIL" = "x"; then
   AC_MSG_ERROR([Cannot find gsutil.])
fi

dnl Test whether the "gcloud" command is somewhere in path.
dnl ---------------------------------------------------------------------------
AC_PATH_PROG([GCLOUD], [gcloud])
if test "x$GCLOUD" = "x"; then
   AC_MSG_ERROR([Cannot find gcloud.])
fi

dnl Test whether the "java" command is somewhere in path.
dnl ---------------------------------------------------------------------------
AC_PATH_PROG([JAVA], [java])
if test "x$JAVA" = "x"; then
   AC_MSG_ERROR([Cannot find java.])
fi

dnl Test whether the "tee" command is somewhere in path.
dnl ---------------------------------------------------------------------------
AC_PATH_PROG([TEE], [tee])
if test "x$TEE" = "x"; then
   AC_MSG_ERROR([Cannot find tee.])
fi

dnl Test whether the "bash" command is somewhere in path.
dnl ---------------------------------------------------------------------------
AC_PATH_PROG([BASH], [bash])
if test "x$BASH" = "x"; then
   AC_MSG_ERROR([Cannot find bash.])
fi

AC_CONFIG_FILES([env], [chmod +x env])
AC_CONFIG_FILES([acontrol], [chmod +x acontrol])
AC_CONFIG_FILES([
  Makefile
  extensions/Makefile
  pcawg/tools.scm
  pcawg/bam-regions.scm
  pcawg/unmapped-reads.scm])

PKG_PROG_PKG_CONFIG
PKG_CHECK_MODULES([htslib], [htslib])
PKG_CHECK_MODULES([guile], [guile-$GUILE_EFFECTIVE_VERSION])

AC_OUTPUT
