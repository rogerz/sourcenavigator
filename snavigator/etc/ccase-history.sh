#! /bin/sh
########################################################################
#
#   ccase-history.sh
#
#   5 May 2004 - E M Thornber
#   Created
#
# Wrapper script for Source Navigator version control interface
# System: ccase.  Argument: -history
#
# Parameters
#   1	File Name
#
echo ""
cleartool lshistory -fmt "Version:%Vn\nDate:%Sd\nLabels:%Nl\nComment:%Nc\n\n" $1
#
# end of ccase-history.sh
########################################################################
