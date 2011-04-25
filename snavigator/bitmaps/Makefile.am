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

imagedir = $(datadir)/snavigator/bitmaps

image_DATA = add.xpm addall.xpm arrow.xpm ascend.xpm \
browse.xpm build.xpm \
classes.xpm compile.xpm \
copy.xpm crossref.xpm cut.xpm \
del.xpm delall.xpm \
down.xpm editor.xbm enabled.xbm \
file.xbm files.xpm find.xpm folder.xbm function.xpm \
go.xpm goto.xpm grep.xpm \
info.xpm insert.xpm \
join.xpm \
left.xpm tilda_left.xpm \
method.xpm minus.xbm move.xpm new.xpm next.xpm \
open.xpm paste.xpm plus.xbm print.xpm project.xpm \
question.xpm \
rarrow.xpm rebuild.xpm right.xpm tilda_right.xpm \
save.xpm search.xpm sign.xpm stop.xpm stree.xpm \
trace.xpm tree.xpm \
undef.xbm undo.xpm up.xpm \
waste.xpm watch.xpm where.xpm \
sn-text.gif \
boxes.xpm clsbr.xpm clsbr_+-.xpm clsbr_+.xpm clsbr_-.xpm clsbr_p+-.xpm \
clsbr_p+.xpm clsbr_p-.xpm clsbr_p.xpm clsbr_pv+-.xpm clsbr_pv+.xpm \
clsbr_pv-.xpm clsbr_pv.xpm clsbr_s.xpm clsbr_v+-.xpm clsbr_v+.xpm \
clsbr_v-.xpm clsbr_v.xpm clsbr_private.xpm \
cross_by.xpm cross_dispparam.xpm cross_param.xpm \
cross_static.xpm cross_to.xpm dbg_redisp.xpm dir.xpm \
file.xpm key.xpm minus.xpm plus.xpm updir.xpm \
type_cl.xpm type_cl+.xpm type_cl-.xpm type_con.xpm \
type_e.xpm type_ec.xpm type_fd.xpm type_fr.xpm type_fu.xpm type_gv.xpm \
type_iv.xpm type_lv.xpm type_ma.xpm type_md.xpm type_mi.xpm type_t.xpm \
type_ud.xpm type_un.xpm undef.xpm enabled.xpm unvisited.xpm \
hold.xpm type_cov.xpm hold_on.xpm hold_off.xpm \
clsbr_s+-.xpm clsbr_s+.xpm clsbr_s-.xpm type_com.xpm \
type_su.xpm parent.xpm filter.xpm \
next2.xpm prev.xpm include.xpm \
file_b.xpm file_s.xpm file_+.xpm file_-.xpm dir_+.xpm dir_-.xpm file_h.xpm \
file_d.xpm unknown.xpm \
play.xpm rplay.xpm sourcenav.ico sn_logo.gif \
splashsn.gif splashsn0.gif splashsn1.gif splashsn2.gif splashsn3.gif

install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(imagedir)
	$(INSTALL_DATA) $(srcdir)/@X11ICON@ $(DESTDIR)$(imagedir)/paf.xbm
