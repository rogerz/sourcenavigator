These patches need to be applied to the sources.redhat.com
CVS before building Source-Navigator. The patches should
be applied from the src directory at the root of the CVS.
The mkdist.sh file in .. also applies these patches
automatically when creating a release.

cd src
patch -p 0 < snavgiator/patches/PATCHFILE

The patch command will need to be run once for each
patch file.
