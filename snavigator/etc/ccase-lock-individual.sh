#! /bin/sh
########################################################################
#
#   ccase-lock-individual.sh
#
#   5 May 2004 - E M Thornber
#   Created
#
# Wrapper script for Source Navigator version control interface
# System: ccase.  Argument: -lock-individual
#
# Parameters
#   1	Version string
#   2	File Name
#
cleartool reserve -nc $2@@$1
#
# end of ccase-lock-individual.sh
########################################################################
