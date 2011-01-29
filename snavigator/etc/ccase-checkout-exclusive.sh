#! /bin/sh
########################################################################
#
#   ccase-checkout-exclusive.sh
#
#   5 May 2004 - E M Thornber
#   Created
#
# Wrapper script for Source Navigator version control interface
# System: ccase.  Argument: -checkout-exclusive
#
# Parameters
#   1	Version string
#   2	File Name
#
cleartool checkout -reserved -nc $2@@$1
#
# end of ccase-checkout-exclusive.sh
########################################################################
