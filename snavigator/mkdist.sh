#!/bin/sh

# This script will create distribution files for a release of
# Source-Navigator. It should be run from the src/ directory
# at the root of a clean CVS checkout.

DIR=/share/SN51/dist
RELEASE=sourcenav-5.1.0
RELEASEDIR=$DIR/$RELEASE

if test ! -d tcl || test ! -d tk ; then
    echo "Must be run from toplevel directory!"
    exit 1
fi

# Apply any needed patches to Tcl CVS modules
rm -f patch.out
PATCHES=`ls snavigator/patches/*.patch`
for file in $PATCHES ; do
    patch -p 0 < $file >> patch.out
done

rm -rf $DIR/$RELEASE
if test ! -d $DIR ; then
    mkdir $DIR
fi
mkdir $RELEASEDIR

# Only copy those files that we actually need
ROOTFILES="COPYING ChangeLog Makefile.in config-ml.in config.guess \
config.if config.sub configure configure.in gettext.m4 install-sh \
libtool.m4 ltcf-c.sh ltcf-cxx.sh ltcf-gcj.sh ltconfig ltmain.sh \
missing mkdep mkinstalldirs move-if-change symlink-tree"

for file in $ROOTFILES ; do
    cp -p $file $RELEASEDIR
done

cp -p -R config db itcl libgui snavigator tcl tix tk $RELEASEDIR

cd $RELEASEDIR

find . -name CVS -exec rm -rf {} \; > /dev/null 2>&1
find . -name ".#*" -exec rm -f {} \; > /dev/null 2>&1
find . -name "*~" -exec rm -f {} \; > /dev/null 2>&1
find . -name "*.rej" -exec rm -f {} \; > /dev/null 2>&1

cp snavigator/README.TXT .
cp snavigator/INSTALL.TXT .

cd ..

# Make a tar.gz archive, we don't need to make
# a zip file of the source since Cygwin is required
# to build and it includes tar and gzip.

tar -czf $RELEASE.tar.gz $RELEASE

echo "Tar $RELEASE.tar.gz saved in $DIR"
