#! /bin/sh
########################################################################
#
#   ccase-checkout-individual.sh
#
#   5 May 2004 - E M Thornber
#   Created
#
# Wrapper script for Source Navigator version control interface
# System: ccase.  Argument: -checkout-individual
#
# Parameters
#   1	Version string
#   2	File Name
#
cleartool checkout -unreserved -nc $2@@$1
#
# end of ccase-checkout-individual.sh
########################################################################
