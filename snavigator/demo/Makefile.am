AUTOMAKE_OPTIONS = cygnus

## Process this file with automake to create Makefile.in.

demosdir = $(datadir)/demos

install-data-local:
	@$(mkinstalldirs) $(DESTDIR)$(demosdir)
	for p in @C_DEMO@ @ASM_DEMO@ @FOR_DEMO@ @COB_DEMO@ @MONOP_DEMO@ @JAV_DEMO@ ; do \
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
