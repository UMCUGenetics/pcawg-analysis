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

AC_INIT([pcawg-analysis], [0.0.1], [roel@gnu.org])
AM_INIT_AUTOMAKE([foreign])
AM_SILENT_RULES([yes])

GUILE_PKG([3.0])
GUILE_PROGS

dnl Installation directories for .scm and .go files.
moddir="${prefix}/share/guile/site/$GUILE_EFFECTIVE_VERSION"
godir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([moddir])
AC_SUBST([godir])

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

dnl Test whether the "samtools" command is somewhere in path.
dnl ---------------------------------------------------------------------------
AC_PATH_PROG([CURL], [curl])
if test "x$CURL" = "x"; then
   AC_MSG_ERROR([Cannot find curl.])
fi

AC_CONFIG_FILES([env], [chmod +x env])
AC_CONFIG_FILES([acontrol], [chmod +x acontrol])
AC_CONFIG_FILES([Makefile pcawg/tools.scm])

AC_OUTPUT
