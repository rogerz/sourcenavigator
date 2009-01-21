      SUBROUTINE COMRUL
      include 'checks.h'
      CCHECK(1) ='Avoid comment lines after end of program unit'
      CCHECK(2) ='End all program program units with the END statement'
      CCHECK(3) =
     & 'Declared COMMON blocks must be used in the program unit'
      CCHECK(4) ='COMPLEX and DOUBLEPRECISION vars at end of COMMON'
      CCHECK(5) ='COMMON block definitions should not change'
      CCHECK(6) ='Variable names should be 6 or fewer characters long'
      CCHECK(7) ='Variables in COMMON should be 6 characters long'
      CCHECK(8) ='Variables not in COMMON should be <6 characters'
      CCHECK(9) ='Integer variables should begin with I to N'
      CCHECK(10)='Variable names should not equal FORTRAN keywords'
      CCHECK(11)='Avoid comment lines before header line'
      CCHECK(12)=
     &'Program unit names should not equal intrinsic function names'
      CCHECK(13)=
     &'Modules should declare IMPLICIT NONE'
      CCHECK(14)=
     &'Program unit should begin with at least 3 comment lines'
      CCHECK(15)='Comment lines should begin with a C or * or !'
      CCHECK(16)='No comment lines between continuations'
      CCHECK(17)='Avoid non-standard variable types eg INTEGER*2'
      CCHECK(18)='Avoid multiple COMMON definitions per line'
      CCHECK(19)='Do not dimension COMMON variables outside COMMON'
      CCHECK(20)='Avoid embedded blanks in variable names'
      CCHECK(21)='Avoid embedded blanks in syntactic entities'
      CCHECK(22)='Avoid the use of PRINT statements (use WRITE)'
      CCHECK(23)='Do not give the END statement a label'
      CCHECK(24)='Avoid WRITE(* construction'
      CCHECK(25)='Avoid WRITE statement in a FUNCTION'
      CCHECK(26)='Avoid the use of PAUSE statements'
      CCHECK(27)='Statement labels should not begin in column 1'
      CCHECK(28)='Always precede STOP by a descriptive WRITE'
      CCHECK(29)='Avoid the use of ENTRY in FUNCTIONS'
      CCHECK(30)='Avoid using I/O in FUNCTIONs'
      CCHECK(31)='Avoid the use of the alternate RETURN statement'
      CCHECK(32)='COMMON block names should not equal variable names'
      CCHECK(33)='Avoid use of obsolete CERN library routines'
      CCHECK(34)='Avoid FUNCTION names the same as intrinsics'
      CCHECK(35)='Local functions should be declared EXTERNAL'
      CCHECK(36)='program unit names should all be different'
      CCHECK(37)='Avoid expressions of mixed mode eg A=B/I'
      CCHECK(38)='Length of passed CHARACTER variables should be *'
      CCHECK(39)='Order of statements should conform to note'
      CCHECK(40)='Separate Statement Functions by comment lines'
      CCHECK(41)='No names in Statement Function definitions elsewhere'
      CCHECK(42)='Use LLT,LGT etc to compare CHARACTER vars. in IFs'
      CCHECK(43)='Variables (not COMMON, not PARAMs) <6 characters'
      CCHECK(44)=
     & 'Passed arguments should be dimensioned * in program unit'
      CCHECK(45)='Variables in COMMON should be >=6 characters long'
      CCHECK(46)='Avoid the use of the ENTRY statement'
      DO 1 IRULE=47,MCHEKS
        CCHECK(IRULE) ='$$$$'
        LCHECK(IRULE) =.FALSE.
    1 CONTINUE
      RETURN
      END
