AUTOMAKE_OPTIONS = cygnus
DEMOS_TO_INSTALL = asm c++ cobol fortran java

## Process this file with automake to create Makefile.in.

demosdir = $(datadir)/snavigator/demos

install-data-local:
	@$(mkinstalldirs) $(DESTDIR)$(demosdir)
	for p in $(DEMOS_TO_INSTALL) ; do \
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
