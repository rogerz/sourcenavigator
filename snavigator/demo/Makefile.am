AUTOMAKE_OPTIONS = cygnus

## Process this file with automake to create Makefile.in.

demosdir = $(datadir)/demos

install-data-local:
	@$(mkinstalldirs) $(DESTDIR)$(demosdir)
	echo CVS > /tmp/sn-install-$$$$ ; \
	for p in @C_DEMO@ @ASM_DEMO@ @FOR_DEMO@ @COB_DEMO@ @MONOP_DEMO@ @JAV_DEMO@ ; do \
	  echo "copying $$p to $(DESTDIR)$(demosdir)" ; \
	  ( \
	  cd $(srcdir) ; \
	  $(TAR) -ch$(OLDTARFLAG) -f /tmp/$$p.tar-$$$$ -X /tmp/sn-install-$$$$ $$p ; \
	  cp /tmp/$$p.tar-$$$$ $(DESTDIR)$(demosdir)/$$p.tar-$$$$ ; \
	  rm /tmp/$$p.tar-$$$$ ; \
	  cd $(DESTDIR)$(demosdir) ; \
	  $(TAR) -xf $$p.tar-$$$$ ; \
	  rm $$p.tar-$$$$ ; \
          ) \
	done ; rm -f /tmp/sn-install-$$$$
