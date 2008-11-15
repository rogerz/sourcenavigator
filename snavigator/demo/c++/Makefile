# $Header$

SUBDIRS = sds editline glish

TAR = glish.tar

all:
	@echo "Building Glish for \$$ARCH ="
	@echo $${ARCH?"You must setenv \$$ARCH"}
	@echo "Creating directories (\"File exists\" errors are okay) ..."
	-@mkdir lib bin include
	-@mkdir lib/$(ARCH) bin/$(ARCH) include/Glish include/Sds
	-@mkdir sds/$(ARCH) editline/$(ARCH) glish/$(ARCH) glish/clients/$(ARCH)
	-@cp install-sh sds
	-@cp install-sh editline
	-@cp install-sh glish
	-@cp install-sh glish/clients
	@$(MAKE) $(MFLAGS) build.sds
	@$(MAKE) $(MFLAGS) build.editline
	@$(MAKE) $(MFLAGS) build.glish

build.sds:
	@cd sds; make $(MFLAGS) install-all

build.editline:
	@cd editline; make $(MFLAGS) install-all

build.glish:
	@cd glish; make $(MFLAGS) install-all

dist:
	$(MAKE) $(MFLAGS) DIST_NAME=glish-`sed <glish/version.h 's/[^"]*"//' | sed 's/"//'` dist2

dist2:
	@rm -rf $(DIST_NAME)
	@rm -f $(DIST_NAME).tar $(DIST_NAME).tar.Z tar-files
	@mkdir $(DIST_NAME)
	$(MAKE) $(MFLAGS) tar-list >tar-files
	tar cf - -I tar-files | (cd $(DIST_NAME); tar xfB -)
	tar cf $(DIST_NAME).tar $(DIST_NAME)
	compress <$(DIST_NAME).tar >$(DIST_NAME).tar.Z
	@rm $(DIST_NAME).tar tar-files

tar-list:
	@ls -d Makefile README NEWS configure.in configure install-sh contrib
	@for i in $(SUBDIRS); do \
		(cd $$i; $(MAKE) $(MFLAGS) tar-list | sed "s,^,$$i/,"); done

rcsinfo:
	@rlog -L -S RCS/*,v
	@for i in $(SUBDIRS); do \
		(cd $$i; $(MAKE) $(MFLAGS) rcsinfo); done

clean-all:
	@for i in $(SUBDIRS); do \
		(cd $$i; $(MAKE) $(MFLAGS) clean-all); done
