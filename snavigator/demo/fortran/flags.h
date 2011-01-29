      COMMON/FLAGS/ACTION(MXFLAG),STATUS(MXFLAG)
      LOGICAL ACTION,STATUS
*-----------------------------------------------------------------------
* +++++++++++++++++++++++++ action flags - as listed
*  1      make namelist/routine
*  2      make global namelist
*  3      print illegal statements
*  4      print changed statements
*  5      print filtered statements
*  6      print all statements
*  7      write changed statements only on output file
*  8      write filtered on output file
*  9      write all on output file
* 10      take first name only in statement
* 11      convert hollerith to quotes
* 12      string replacement requested
* 13      resequence statement numbers
* 14      FORMAT to end of routine
* 15      name replacements requested
* 16      routine filters given
* 17      class filters given
* 18      name filters given
* 19      string filters given
* 20      type variables
* 21      indent
* 22      USER command given
* 23      compressed output file requested
* 24      COMMON block option (signal unused and used C.B.)
* 25      print namelist / routine
* 26      print global namelist
* 27      print COMMON block and variable usage
* 28      adjust GOTO to the right
* 29      write tree output file on unit 13
* 30      write HTML output on unit 80
* +++++++++++++++++++++++++ status flags - as listed
*  1      no more lines on input
*  2      no more lines to process
*  3      illegal stmnt. detected in EXTRAC (unclosed string, or
*         illegal character '{', '}'  ).
*  4      end of program due to time limit
*  5      currently buffered routine without end (split)
*  6      currently buffered routine continuation (split)
*  7      current routine filtered
*  8      last filter passed
*  9      routine header still to be printed
* 10      statement still to be printed
* 11      statement cannot be changed (length overflow,or illegal repl.)
* 12      c.b. name list overflow in PROCOM, discard current routine
* 13      true when equiv. groups and commons have been merged (PROCOM)
* 14      true when current routine is a SUBROUTINE
*-----------------------------------------------------------------------
