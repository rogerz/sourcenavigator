/* $Header: */
#ifndef ISTKsds_errors_h
#define ISTKsds_errors_h

#define ERRSTACK 6
#define ERRSTRINGLEN 128

/*    Error returns        */

#define SDS_NO_SUCH_SDS          -1
#define SDS_NO_SPC               -2
#define SDS_FILE_OP              -3
#define SDS_FILE_WR              -4
#define SDS_NO_SUCH_OBJ          -5
#define SDS_FILE_RD              -6
#define SDS_NOT_SDS              -7
#define SDS_BAD_VERSION          -8
#define SDS_FILE_NOP             -9
#define SDS_SWAPPED_BYTES        -10
#define SDS_NOT_ASSEMBL          -11
#define SDS_NOT_INITIALISED      -12
#define SDS_UNDEFINED_TYPE       -13
#define SDS_NOT_DEFINABLE        -14
#define SDS_ALREADY_EXISTS       -15 
#define SDS_DEJA_LA              SDS_ALREADY_EXISTS
#define SDS_TRANSFER_UNDEF       -16
#define SDS_WRONG_TYPE           -17
#define SDS_WRONG_PADS           -18
#define SDS_NO_MEM               -19
#define SDS_NO_DB_PROC           -20
#define SDS_DB_ACCESS            -21
#define SDS_NOT_COMPLEX_OBJECT   -22
#define SDS_WRONG_RES_LIST       -23
#define SDS_ZERO_LENGTH          -24
#define SDS_GOOD_FORMAT          -25
#define SDS_CANNOT_RESIZE        -26
#define SDS_END_RES_STACK        -27
#define SDS_NO_MAP_FILE          -28
#define SDS_NO_SUCH_LIST         -29
#define SDS_BAD_ERROR_REGISTERED -30
#define SDS_USER_MARK            -31
#define SDS_NOT_CANNED           -32
#define SDS_NAMELIST             -33
#define SDS_DATA_UNUSED          -34
#define SDS_ZERO_READ            -35

#define SDS_MAX_ERR               35

#define SDS_BE_QUIET             -1
#define SDS_LINE_MARK             0
#define SDS_RETURN                1
#define SDS_WARNING               3
#define SDS_ERROR                 5 
#define SDS_FATAL                 9

#define sds_push_error(errcode, errlevel, errstring)            \
sds_push_lerror(errcode, errlevel, errstring,__LINE__, __FILE__)

#define sds_mark(comment)            \
sds_lmark(comment,__LINE__, __FILE__)

struct sds_err {
  int errcode;
  int errlevel;
  char errstring[ERRSTRINGLEN];
  char filename[ERRSTRINGLEN];
  int line;
};

struct sds_error_control {
  int line;
  char filename[ERRSTRINGLEN];
  int errstack;
  int stack_level;
  int exit_on_fatal;
  int stack_overwritten;
  int output_level;
  int proginfo;
  struct sds_err *se;
};

EXTERN int         sds_push_lerror(int,int,char *,int, char*);
EXTERN int         sds_lmark(char *,int,char *);
EXTERN int         sds_last_error(void);
EXTERN int         sds_last_warning(void);
EXTERN int         sds_last_return(void);
EXTERN void        sds_output_proginfo(int);
EXTERN void        sds_clear_errors(void);
EXTERN void        sds_output_errors(int);
EXTERN void        sds_stop_if_error(char *);
EXTERN void        sds_exit_on_fatal(int);

#endif
