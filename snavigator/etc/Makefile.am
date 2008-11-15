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

LANG_TXT_FILES=english.txt french.txt german.txt

## Process this file with automake to produce Makefile.in.
SUBDIRS = sn_toolchains

etcdir = $(datadir)/snavigator/etc
etc_DATA = $(LANG_TXT_FILES) grep.fil sn_prop.cfg \
sn.el sn_cmp_g.pat snscenario snavigator snpdbg

# +EMT 070415
# Impedance matching scripts for Clearcase commands
libexec_SCRIPTS = ccase-checkout-exclusive.sh \
ccase-checkout-individual-to-stdout.sh \
ccase-checkout-individual.sh \
ccase-checkout.sh \
ccase-history-individual.sh \
ccase-history.sh \
ccase-lock-individual.sh \
ccase-unlock-individual.sh
# -EMT 070415

#all-local:	stamp

## Compare all language files against English.  english.txt must be first in
## the dependency list.

stamp:		$(LANG_TXT_FILES)
		TCL_LIBRARY=$(TCL_LIBRARY); export TCL_LIBRARY; \
                for lang in $^ ;\
		do \
		  here=`pwd` ;\
		  @ITCL_SH@ $(top_srcdir)/misc/tools/verify_texts.tcl $$lang $< ;\
		done
		touch stamp

