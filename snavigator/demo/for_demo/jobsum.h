      COMMON/JOBSUM/TIME1,TIME2,NSTATC(10),NFDCLS(MXSTAT,2)
*-----------------------------------------------------------------------
*    contains the statistical information
*       TIME1     starting time in seconds
*       TIME2     ending     -        -
*       NSTATC    overall statistical information
*                    1 = # of lines read
*                    2 = # of lines written to output file
*                    3 = # of statements read
*                    4 = # of statements after filters
*                    5 = # of statements changed
*                    6 = # of lines unable to change (length overflow)
*                    7 = # of comment lines (including blank lines)
*                    8 = # of lines printed
*       NFDCLS   no. of times internal class found
*                (I,1)  normal, (I,2) behind logical IF
*-----------------------------------------------------------------------
