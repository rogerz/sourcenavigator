# Copyright (c) 2000, Red Hat, Inc.
# 
# This file is part of Source-Navigator.
# 
# Source-Navigator is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# Source-Navigator is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with Source-Navigator; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.
# 
AUTOMAKE_OPTIONS = cygnus

## Process this file with automake to create Makefile.in.

htmldir = $(prefix)/share/snavigator/html
html_DATA=cygnus.css index-1.gif magnifying_glass.png index.html
srchtmldir = $(top_srcdir)/doc/html

install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(htmldir)
	for p in @SN_BOOKS@; do \
		$(mkinstalldirs) $(DESTDIR)$(htmldir)/$$p; \
		rm -f $(DESTDIR)$(htmldir)/$$p/*.html; \
		rm -f $(DESTDIR)$(htmldir)/$$p/*.gif; \
		echo "copying $$p to $(DESTDIR)$(htmldir)/$$p" ;\
		( \
		cd $(srcdir)/@SN_DOCSET@/$$p ;\
		$(TAR) -ch$(OLDTARFLAG) -f /tmp/$$p.tar-$$$$ *.html *.gif ;\
		cp /tmp/$$p.tar-$$$$ $(DESTDIR)$(htmldir)/$$p ;\
		rm /tmp/$$p.tar-$$$$ ;\
		cd $(DESTDIR)$(htmldir)/$$p ;\
		$(TAR) -xf $$p.tar-$$$$ ;\
		rm $$p.tar-$$$$ ;\
		) \
	done;
