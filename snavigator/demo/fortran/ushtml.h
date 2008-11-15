      PARAMETER (MXANCH=20000)
      COMMON /USHTML/ html,anchor_list,nanchr
      COMMON /USHTMC/ canchr(MXANCH),cancht(MXANCH)
      LOGICAL html
      integer anchor_list
      character*(mxnmch) canchr
      character*(80) cancht
*-----------------------------------------------------------------------
*   HTML        = set .TRUE. if generation of HTML required
*   ANCHOR_LIST = 0  Do not generate or use Anchor dictionary
*                 1  Generate dictionary on MDUNIT
*                 2  Use dictionary on MDUNIT
*   MXANCH      = maximum number of anchor points allowed
*   NANCHR      = number of anchor points in dictionary
*   CANCHR      = Fortran variables for which there are anchors
*   CANCHT      = equivalent anchor name for each Fortran variable
*-----------------------------------------------------------------------
