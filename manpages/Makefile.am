# befstools manpages/Makefile.am
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

man_MANS = mkfs.befs.8


if COMPAT_SYMLINKS
install-data-hook:
	cd $(DESTDIR)$(mandir)/man8 && $(LN_S) -f mkfs.befs.8 mkbefs.8

uninstall-hook:
	$(RM) $(DESTDIR)$(mandir)/man8/mkfs.befs.8
endif
