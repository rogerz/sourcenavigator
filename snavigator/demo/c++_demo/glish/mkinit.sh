#! /bin/sh

cat <<!
// File created from glish.init via mkinit.sh.

extern const char* glish_init[];

const char* glish_init[] = {
!

sed 's/\\/&&/g' $* | sed 's/"/\\"/g' | sed 's/.*/  "&",/'

cat <<!
  0
};
!
