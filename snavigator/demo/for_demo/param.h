      PARAMETER(MXNAME=30000,MXSSTM=2000,MXSTAT=75,MCLASS=22,MXLINE=80,
     1  MXLENG=1320,MXSIMA=2000,MXSIMD=MXSIMA+500,MCUNIT=9,MPUNIT=6,
     2  MIUNIT=11,MTUNIT=13,MOUNIT=14,MXFLAG=30,MXNMCH=32,MXORST=20,
     3  MDIMST=2000,MGLOKY=9,MLOCKY=4,MSUBKY=24,MTOTKY=MGLOKY+MLOCKY,
     4  MXKEYS=MGLOKY+MXORST*MLOCKY,MXKINT=100,MXKNAM=500,MXTYPE=20,
     5  MAXNUM=1000,MAXGRP=100,TIMLIM=1.,MHUNIT=80,MDUNIT=81,
     +  VERSIO=7.00,
     6  KALL=100,KENT=20,NOARG=50)
*-----------------------------------------------------------------------
*--- MXNAME = dimension of IWS, COMMON/FLWORK/, and of SNAMES /ALCAZA/
*    MXSSTM = length of string SSTM, COMMON/ALCAZA/
*    MXSTAT = max. no. of statement definitions
*    MCLASS = first dim. of ISTMDS( , ) = no. of control words/statement
*    MXLENG = max. length of statement field (20*66)
*    MXLINE = line length of input image
*    MXSIMA = max. no. of lines in input image (one routine)
*    MXSIMD = dim. of SIMA (excess for replacement overflows)
*    MCUNIT = file for command input (data cards)
*    MPUNIT = file for printed output
*    MIUNIT = FORTRAN code input unit
*    MTUNIT = TREE output unit
*    MOUNIT = FORTRAN code output unit
*    MHUNIT = HTML source with Anchors output unit
*    MDUNIT = Dictionary for anchor point translations
*    MXFLAG = no. of status and action flags
*    MXNMCH = max. no. of characters per name
*    MXORST = max. no. of OR-sets in control commands
*    MDIMST = dimension of SSTA, SSTR, SKYSTR
*    MGLOKY = no. of global command keys
*    MLOCKY = no. of local (in each OR-set) command keys
*    MSUBKY = no. of command sub-keys
*    MXKINT = dim. of KEYINT  /KEYINP/
*    MXKNAM = max. no. of names or strings on input commands (total)
*    MXTYPE = max. no. of variable types
*    MAXNUM = max. no. of statement numbers per routine
*    MAXGRP = max. no. of c.b. names or equiv. groups (for ACTION(24))
*    TIMLIM = if less time left, refrain from reading next routine
*    VERSIO = program version
*    KALL   = max. no. of different externals / routine (TREE)
*    KENT   = max. no. of ENTRY statements / routine    (TREE)
*    NOARG  = max. no. of arguments / call              (TREE)
*-----------------------------------------------------------------------
