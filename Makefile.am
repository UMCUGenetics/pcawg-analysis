# Copyright © 2020 Roel Janssen <roel@gnu.org>
#
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

AUTOMAKE_OPTIONS        = subdir-objects
SUBDIRS                 = extensions

include $(top_srcdir)/build-aux/guile.am

SOURCES =                  \
  logger.scm               \
  ui/panel-file.scm        \
  ui/remove-donor.scm      \
  ui/postprocess.scm       \
  ui/preprocess.scm        \
  pcawg/config.scm         \
  pcawg/dcc-portal.scm     \
  pcawg/google.scm         \
  pcawg/unmapped-reads.scm \
  pcawg/hartwig.scm        \
  pcawg/filesystem.scm     \
  pcawg/tools.scm

EXTRA_DIST     += pcawg/tools.scm.in          \
                  pcawg/unmapped-reads.scm.in \
                  acontrol.in                 \
                  env.in

bin_SCRIPTS     = acontrol

# Build docker image with Guix
docker: dist
	guix pack -C gzip -e '(load "environment.scm")' \
             --with-source=pcawg-analysis-$(VERSION).tar.gz \
             --format=docker \
             -S /bin=bin -S /lib=lib -S /share=share -S /etc=etc -S /var=var \
             glibc-utf8-locales tzdata coreutils bash grep gawk net-base strace \
             python@2 google-cloud-sdk hmf-google-pipeline

pack: dist
	guix pack -C gzip -e '(load "environment.scm")' \
             --with-source=pcawg-analysis-$(VERSION).tar.gz \
             -S /bin=bin -S /share=share -S /lib=lib \
             glibc-utf8-locales tzdata coreutils bash grep gawk net-base strace \
             python@2 google-cloud-sdk hmf-google-pipeline guile emacs-geiser
