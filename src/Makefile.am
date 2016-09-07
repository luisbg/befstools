# befstools src/Makefile.am
# Copyright (C) 2016  Luis de Bethencourt <luisbg@osg.samsung.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

AM_CFLAGS = -Wall -Wextra -Wno-sign-compare -Wno-missing-field-initializers \
	    -Wmissing-prototypes -Wstrict-prototypes -Wwrite-strings

sbin_PROGRAMS = mkfs.befs befslabel
noinst_PROGRAMS = testdevinfo

mkfs_common_sources = device_info.c device_info.h \
		      blkdev/blkdev.c blkdev/blkdev.h \
		      endian_compat.h
mkfs_befs_SOURCES  = mkfs.befs.c befs.h $(mkfs_common_sources)
mkfs_befs_CPPFLAGS = -I$(srcdir)/blkdev
mkfs_befs_CFLAGS   = $(AM_CFLAGS) $(UDEV_CFLAGS)
mkfs_befs_LDFLAGS  = $(UDEV_LIBS)

befslabel_SOURCES = befslabel.c common.c common.h befs.h io.c io.h

testdevinfo_SOURCES  = testdevinfo.c $(mkfs_common_sources)
testdevinfo_CPPFLAGS = -I$(srcdir)/blkdev
testdevinfo_CFLAGS   = $(AM_CFLAGS) $(UDEV_CFLAGS)
testdevinfo_LDFLAGS  = $(UDEV_LIBS)


if COMPAT_SYMLINKS
install-exec-hook:
	cd $(DESTDIR)$(sbindir) && $(LN_S) -f mkfs.befs mkbefs

uninstall-hook:
endif
