#! /bin/sh
########################################################################
#
#   ccase-unlock-individual.sh
#
#   5 May 2004 - E M Thornber
#   Created
#
# Wrapper script for Source Navigator version control interface
# System: ccase.  Argument: -unlock-individual
#
# Parameters
#   1	Version string
#   2	File Name
#
cleartool unreserve -nc $2@@$1
#
# end of ccase-unlock-individual.sh
########################################################################
