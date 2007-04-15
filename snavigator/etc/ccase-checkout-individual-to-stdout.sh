#! /bin/sh
########################################################################
#
#   ccase-checkout-individual-to-stdout.sh
#
#   5 May 2004 - E M Thornber
#   Created
#
# Wrapper script for Source Navigator version control interface
# System: ccase.  Argument: -checkout-individual-to-stdout
#
# Parameters
#   1	Version string
#   2	File Name
#
cat $2@@$1
#
# end of ccase-checkout-individual-to-stdout.sh
########################################################################
