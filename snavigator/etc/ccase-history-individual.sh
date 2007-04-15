#! /bin/sh
########################################################################
#
#   ccase-history-individual.sh
#
#   5 May 2004 - E M Thornber
#   Created
#
# Wrapper script for Source Navigator version control interface
# System: ccase.  Argument: -history-individual
#
# Parameters
#   1	Version string
#   2	File Name
#
echo ""
cleartool describe -fmt "Version:%Vn\nDate:%Sd\nLabels:%Nl\nComment:%Nc\n\n" -version $1 $2
#
# end of ccase-history-individual.sh
########################################################################
