#!/bin/sh

# This script will create distribution files for a release of
# Source-Navigator. It should be run from the src/ directory
# at the root of a clean CVS checkout.

DIR=/share/SN51/dist

if test ! -d tcl || test ! -d tk ; then
    echo "Must be run from toplevel directory!"
    exit 1
fi

# Get the version number for this release.

VERSION=`grep AM_INIT_AUTOMAKE snavigator/configure.in`
if test "$VERSION" != "" ; then
    VERSION=`expr substr $VERSION 21 5`
    if grep VERSION=$VERSION snavigator/configure > /dev/null; then
        DO=nothing
    else
        echo "configure.in version does not match version in configure script"
        exit 2
    fi
else
    echo "version not found in configure.in"
    exit 1
fi

RELEASE=sourcenav-${VERSION}
RELEASEDIR=$DIR/$RELEASE

# Apply any needed patches to Tcl CVS modules
rm -f patch.out
PATCHES=`ls snavigator/patches/*.patch 2>/dev/null`
for file in $PATCHES ; do
    patch -p 0 < $file >> patch.out
done

rm -rf $DIR/$RELEASE
if test ! -d $DIR ; then
    mkdir $DIR
fi
mkdir $RELEASEDIR

# Only copy those files that we actually need
ROOTFILES="COPYING ChangeLog Makefile.in config.guess \
config.sub configure configure.in install-sh \
missing mkinstalldirs"

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
cp snavigator/NEWS .

cd ..

# Make a tar.gz archive, we don't need to make
# a zip file of the source since Cygwin is required
# to build and it includes tar and gzip.

tar -czf $RELEASE.tar.gz $RELEASE

echo "Tar $RELEASE.tar.gz saved in $DIR"
