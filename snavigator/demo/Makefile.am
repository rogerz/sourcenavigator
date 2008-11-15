AUTOMAKE_OPTIONS = cygnus
DEMOS_TO_INSTALL = asm_demo c++_demo cob_demo for_demo jav_demo monop

## Process this file with automake to create Makefile.in.

demosdir = $(datadir)/snavigator/demos

install-data-local:
	@$(mkinstalldirs) $(DESTDIR)$(demosdir)
	for p in $(DEMOS_TO_INSTALL) ; do \
	  test -d "$p" || continue
	  echo "copying $$p to $(DESTDIR)$(demosdir)" ; \
	  ( \
	  cd $(srcdir) ; \
	  $(TAR) -ch$(OLDTARFLAG)f /tmp/$$p.tar-$$$$ $$p ; \
	  cp /tmp/$$p.tar-$$$$ $(DESTDIR)$(demosdir)/$$p.tar-$$$$ ; \
	  rm /tmp/$$p.tar-$$$$ ; \
	  cd $(DESTDIR)$(demosdir) ; \
	  $(TAR) -xf $$p.tar-$$$$ ; \
	  rm $$p.tar-$$$$ ; \
          ) \
	done
	find $(DESTDIR)$(demosdir) -name CVS > cvs_files
	for p in `cat cvs_files` ; do \
	    rm -rf $$p ; \
	done
	rm -f cvs_files
