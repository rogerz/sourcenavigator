#ifndef ISTKdirec_h
#define ISTKdirec_h     


/**************************************************************************
 *                 ****** ISTK Release 1.2 *****                          *
 *                                                                        *
 *                                                                        *
 * This code has been produced by numerous authors at the CERN centre for *
 * high energy physics, Geneve, Switzerland, at the SSC laboratory in     *
 * Dallas, Texas, USA and at the Lawrence Berekeley Laboratory in         *
 * California, USA.                                                       *
 * The latter two institutions perform work under US Government contract. *
 * The intent of the work is to provide useful code for people who need   *
 * it, with an emphasis on free and collaborative exchange of ideas,      *
 * techniques and implementations.                                        *
 * Please read the disclaimer and copyright notices contained in the ISTK *
 * distribution and in distributed applications.                          *
 *                                                                        *
 **************************************************************************/


/* Reference release  Aug 10 1991 - C G Saltmarsh */
/* Has the basics used at CDG & SSC 1988-1991, plus vxworks
   support
*/

/************************************************************************
 *            Copyright (C) Frogsoft Corporation,  1987                 *
 *
 * SSC VERSION 2.0001 April 29 1988
 *
 * Modification history:
 *
 * 2.0100 Base release Dec 20 1990 
 *
 ***********************************************************************/

#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif

#if defined (__STDC__)
#define VOIDDEF void
#else
#define VOIDDEF /* void */
#endif
     
typedef char * sds_addr; 
typedef long   sds_off_t; 

#if (SDS_BIGADDR != 1) /* this will have been set in sds_mdpe.h */
typedef long          sds_handle;
typedef unsigned long sds_code;
#define SDS_HFORM %ld
#else
typedef int            sds_handle;
typedef unsigned int   sds_code;
#define SDS_HFORM %d
#endif

struct record_entry {
	sds_code number;
	sds_code elemtype;
	void     *pointer;
	struct record_entry *next;
	char     *name;
	int      record_depth;
	};

typedef struct record_header sds_record_handle;

      /*  a few NULL pointers */

#define DNULL ((struct direc *)0)
#define TNULL ((struct type_list *)0)
#define HNULL ((struct sds_header *)0)
#define INULL ((int *)0)
#define SNULL ((char *)0)

#define SDS_ALLOCATE ((sds_off_t )0)

#define SDS_NOT_ASSEMBLED      (sds_off_t)-1
#define SDS_IMPOSSIBLE_ADDRESS (sds_off_t )-2

#ifdef  VMS
#define SDS_BLOCKING_FACTOR 512
#else
#define SDS_BLOCKING_FACTOR 8192
#endif
#define SDS_DEFAULT_BUFSIZE 0x10000

#ifndef L_SET
#define L_SET 0
#endif
     

#define SDS_BASE_MAGIC  (sds_code )0x50420043
#define SDS_MAGIC  SDS_BASE_MAGIC + (sds_code )((SDS_ARC+1) << 8)
#define SDS_MAGIC_BYTESWAP (sds_code )0x43004250
#define SDS_VERSION  (short )3

/* the following are markers to define the end of a tlist... */

#define SDS_ENDLIST  ((sds_code )0x40000001)

/* ... the end of a structure within a tlist...       */

#define SDS_RETLIST  ((sds_code )0x40000000)

/* ...and a flag to be &'ed with an internal (not system) pointer
 to generate a non-primitive element code.                   */

#define SDS_INDLIST    ((sds_code )0x80000000)
#define SDS_STRUCT     SDS_INDLIST
#define SDS_LOCALLIST  ((sds_code )0x08000000)
#define SDS_CODE_MASK  ((sds_code )0xff000000)
#define SDS_UNION      ((sds_code )0x04000000L)
#define SDS_RECLIST    ((sds_code )0x02000000L)

/* this, at the start of a tlist,indicates that it is named   */

#define SDS_LENLIST    ((sds_code )0x10000000)
#define SDS_SIZE_ALIGN ((sds_code )0x20000000)

/* These things should be hidden from the user as soon as possible */

#define SDS_WAS_ALLOCATED   (char)0x01
#define SDS_EXTERNAL_OBJECT (char)0x02
#define SDS_REALLOC         (char)0x04
#define SDS_DISJOINT_OBJECT (char)0x08

#define SDS_SAVE_STATE 1
#define SDS_RESTORE_STATE 2

#define SDS_GEN_NAME (sds_code )-1

#define SDS_READY -1
     
#define   DIR_NAM_LEN   32

#define SDS_HEAP_ALLOC   0x01
#define SDS_TLIST_ALLOC  0x02
#define SDS_DPTR_ALLOC   0x04
#define SDS_HEAD_ALLOC   0x08
#define SDS_DUP_ALLOC    0x10

#define SDS_NORMAL_OBJECT   (short)0x00
#define SDS_LISTK           (short)0x01
#define SDS_VARIABLE_LENGTH (short)0x02
#define SDS_RECORDS         (short)0x04
#define SDS_FORTRAN_RECORDS (short)0x08
#define SDS_SDC_EVENT       (short)0x10
#define SDS_EXTERNAL_LENGTH (short)0x20

#define SDS_LENGTH_UNDETERMINED 0xffffffff
#define SDS_LOADED_MULT         0xfffffffe

#define SDS_DATASET_LEVEL   0
#define SDS_OBJECT_LEVEL    1
#define SDS_SUBOBJECT_LEVEL 2


/* Here are things that go in the controlbits region of SDS control */
/* First, what to do if you find incoming 64-bit integers */
#define SDS_DOUBLE_LONGS_IN_LONGS 0x0
#define SDS_TRUNCATE_DOUBLE_LONGS 0x1
#define SDS_FLOAT_DOUBLE_LONGS 0x2
#define SDS_TRAP_DOUBLE_LONG_OVERFLOW 0x4
#define SDS_DOUBLE_LONG_MASK 0x3

/* This flag is set in the PER-SDS controlbits: that is, it signals
   incoming big address data so that offset sizes in eg direc.h must be
   truncated */
#define SDS_FROM_BIGADDR 1

/* This bit says that a dataset is actually risc-padded even if
	 it was actually generated on an intel or 680x0 machine. */
#define SDS_IS_RISCY 0x80



struct sds_header {  
  sds_code magic_number; 
  short controlbits; 
  short version; 
  unsigned short heap_size; 
  unsigned short list_size; 
 };

#define BASE_OFFSET sizeof(struct sds_header)
     
struct direc    {            /* Directory structure for standard dataset*/
 sds_off_t       offst;      /* offset from memory start */
 sds_code        nelems;     /* # elements */
 sds_code        elemsz;     /* size in bytes of 1 element */
 sds_code        elemcod;    /* code of the type */
 sds_code        wtime;      /* last write time  */
 short           structype;  /* is it a list etc? */
 unsigned char   align_type; /* alignment type  */
 unsigned char   illoca;     /* reallocation flag */
 sds_code        obj_name;   /* name offset of the object */
 };

struct type_list {
 sds_code        nelems;    /* # elements requested */
 sds_code        elemcod;   /* code of the type */
 };

struct    sds_odesc {
 char *   address;       /* start address */
 char *   name;          /* base name */
 sds_code elemcod;       /* code */
 sds_code nelems;        /* number of elements */
 sds_code nnames;        /* number of names   */
 sds_code size;          /* size of one element, in bytes */
 char     align;         /* Byte boundary for alignment */
 char     startbit;
 char     nbits;
 char *   namelist;      /* Pointer to namelist of nnames names */
 sds_code maxn;          /* system bookkeeping */
 sds_code ind;           /* system bookkeeping */
 };

struct  sds_res_control {
  int                 stack_size;
  int                 addr_inc;
  int                 thiso;
  struct  sds_odesc  *start_stack;
  struct  sds_odesc  *cette;
  struct  sds_odesc  *parent;
  struct  type_list  *tptr;
  struct  type_list   tdum[2];
  char               *nheap;
  char               *sds_al;
  char               *base_address;
  struct record_entry *r;
  int                 firstpass:1;
  int                 done:1;
};

typedef struct sds_res_control rescon;

struct sds_control_p {
    short                 allofl;
    int                   stream;
    int                   tstream;
    int                   source;
    int                   heap_size;
    int                   direc_size;
    int                   tree;
    int                 **varel_count;
    sds_handle           *file_offsets;
    sds_handle           *dup_size;
    char                 *load_name;
    char                 *target_name;
    char                 *heap;
    char                 *current_heap;
    char               ***element_start;
    struct sds_header    *shead;
    struct type_list     *tlist;
    struct direc         *dptr;
    struct sds_res_control *src;
    char                 genarc;
    sds_code             is_proto : 1;
};

struct sds_saverestore
{
  char       *sds_state;
  sds_handle  status;
  char      **opointer;
};

#if defined(SHMEM)
/* I have Unix shared memory      */
EXTERN char   *shm_make(char *, int, int);
EXTERN struct  direc *shm_attr(char *);
EXTERN struct  direc *shm_attw(char *);
#else
#endif

EXTERN off_t      ffsiz(int);

EXTERN int        sds_max                (void);
EXTERN int        sds_errstack           (void);
EXTERN struct     sds_listcon * sds_listc    (void);
EXTERN struct     list_control *sds_reglist(void);
EXTERN void       sds_trap_bigint_overflow(int);
EXTERN void       sds_truncate_bigints(void);
EXTERN void       sds_float_bigints(void);
EXTERN void       sds_pack_bigints(void);
EXTERN void       sds_bigint_trap(void *);
EXTERN int        sds_query_truncate_b(void);
EXTERN int        sds_query_trap_b(void);
EXTERN int        sds_query_float_b(void);
EXTERN int        sds_query_pack_b(void);

EXTERN struct sds_saverestore * sds_saver(void);

EXTERN int        sds_target_file(sds_handle, char *);
EXTERN int        sds_load_object(sds_handle,sds_handle,char *);
EXTERN int        sds_set_riscpad(sds_handle);
EXTERN char       sds_rbyte             (void);
EXTERN char       sds_arc_rbyte         (int);
EXTERN char       sds_palign            (sds_code);
EXTERN char     * sds_arc_aligns        (int);
EXTERN char       sds_psize             (sds_code);
EXTERN char     * sds_typename          (sds_code);
EXTERN char     * sds_c_typename        (sds_code);
EXTERN char       sds_arc_palign        (int, sds_code);
EXTERN int        sds_initialised       (void);
EXTERN int        sds_initialise_enabled(void);
EXTERN int        sds_maxbufsize      (void);
EXTERN void       sds_setmaxbuf       (int);
EXTERN int        sds_close_fd(int fd);
EXTERN int        sds_read(int fd, int size, char *buffer);

EXTERN struct sds_control_p * sds_control(sds_handle);

EXTERN int         sds_dataset_check   (sds_handle);
EXTERN int         sds_stream          (sds_handle);
EXTERN int         sds_genarc          (sds_handle);
EXTERN int         sds_source          (sds_handle);
EXTERN int         sds_heap_size       (sds_handle);
EXTERN int         sds_direc_size      (sds_handle);
EXTERN int      ** sds_varel           (sds_handle);
EXTERN sds_handle* sds_dup_size        (sds_handle);
EXTERN char      * sds_loadname        (sds_handle);
EXTERN char      * sds_heap            (sds_handle);
EXTERN char      * sds_current_heap    (sds_handle);
EXTERN char    *** sds_estart          (sds_handle);
EXTERN short       sds_alloflag        (sds_handle);

EXTERN int         sds_open_file       (char *, int);

EXTERN struct sds_header * sds_head   (sds_handle);
EXTERN struct direc      * sds_direc  (sds_handle);
EXTERN struct type_list  * sds_tlist  (sds_handle);
EXTERN rescon            * sds_src(sds_handle);
EXTERN rescon            *sds_new_rescon();
EXTERN void               sds_delete_rescon(rescon *);

EXTERN void       sds_discard(sds_handle);
EXTERN void       sds_destroy(sds_handle);

EXTERN sds_handle sds_map(char *, sds_code);
EXTERN sds_handle sds_adaptive_map(char *,int,sds_code,char **,off_t *,off_t *);
EXTERN sds_handle sds_na_load(sds_handle,int,struct sds_header*);

EXTERN sds_handle sds_mapfilecheck(char *, int, int);
EXTERN char *     sds_searchmapfile(sds_handle, int *, int *);


EXTERN void sds_print_record_def(sds_record_handle *);
EXTERN void sds_destroy_record_def(sds_record_handle *, int);
EXTERN void sds_record_entry(sds_record_handle *,sds_code,
                                    sds_code,void *, char *);
EXTERN void sds_end_sub_record(sds_record_handle *);
EXTERN void sds_begin_sub_record(sds_record_handle *,char *);
EXTERN sds_record_handle * sds_begin_record(char *);
EXTERN sds_handle sds_end_and_declare(sds_record_handle *, sds_handle);
EXTERN int sds_copy_records(sds_handle,char *, sds_record_handle *);
EXTERN sds_handle sds_write_records(sds_handle, int, sds_record_handle *,
                                                                                         char *,int,int *);

EXTERN sds_handle sds_cload_direc(sds_handle, int,sds_handle*,struct sds_header*);
EXTERN sds_handle sds_load_conv(char *);
EXTERN sds_handle sds_cload(int);

EXTERN void       sds_perror(char *);

EXTERN sds_handle sds_list(sds_handle, sds_code, sds_code);
EXTERN void       sds_printit(sds_code ,sds_code ,char *,char,char);

EXTERN void       *sds_obname2ptr(sds_handle , char *);
EXTERN void       *sds_obind2ptr(sds_handle ,sds_code );
EXTERN char       *sds_obind2name(sds_handle , sds_code );
EXTERN sds_handle sds_obind2code(sds_handle , sds_handle );

EXTERN sds_handle sds_array_size(sds_handle ,sds_handle );

EXTERN sds_handle sds_bad_object(sds_handle ,sds_code );
EXTERN sds_handle sds_get_checked(sds_handle ,char *,sds_code );

EXTERN sds_handle sds_tstamp(sds_handle, sds_handle);
EXTERN long      *sds_get_tstamp(sds_handle, sds_handle);

EXTERN float      sds_version(sds_handle );

EXTERN sds_handle sds_duplicate_def(sds_handle, sds_handle, sds_code);
EXTERN sds_handle sds_resize_object(sds_handle,sds_code,sds_code);

EXTERN void       sds_init(void);
EXTERN void       sds_global_init(int, int);
EXTERN void       sds_reinit_enable(void);

EXTERN int        sds_describe(sds_handle, sds_code, struct sds_odesc **);
EXTERN int        sds_resolve(sds_handle, sds_code,
                                         struct sds_odesc **, sds_code);
EXTERN int        sds_peek_endstruct(void);
EXTERN sds_handle sds_find_thing(sds_handle, char *,struct sds_odesc **);
EXTERN void       sds_cleanup(sds_handle);
EXTERN int        sds_make_name(char *, struct sds_odesc *, int, int);

EXTERN sds_handle good_sds(char *);

EXTERN char      *sds_oname(sds_handle,sds_code,sds_code);

EXTERN sds_handle sds_element_size(sds_handle,sds_code,unsigned char *);
EXTERN sds_handle sds_sizeof_object(sds_handle,sds_code);
EXTERN sds_handle sds_set_object_type(sds_handle, sds_handle,int);
EXTERN sds_handle sds_get_object_type(sds_handle,sds_handle);
EXTERN sds_handle sds_get_object_location(sds_handle,sds_handle, int);
EXTERN sds_handle sds_set_object_location(sds_handle,sds_handle, int);
EXTERN sds_code   sds_get_size(sds_handle, sds_code);
EXTERN char       sds_get_align(sds_handle, sds_code);


EXTERN int        shm_q(char *);
EXTERN int        shm_destroy(char *);
EXTERN int        shm_quit(struct direc *);
EXTERN int        shm_sz(char *);
EXTERN int        shm_lock(char *, int);

EXTERN sds_handle sds_header_ok(struct sds_header *);

EXTERN sds_handle sds_flat_setup(sds_handle,
                                sds_code, int, sds_code *, void *);
EXTERN sds_handle sds_flat_read(sds_handle, sds_code, sds_code , int);

EXTERN sds_handle sds_twod_declare(sds_handle, void *, char *,
                                              sds_code, sds_code, sds_code);
EXTERN sds_handle sds_threed_declare(sds_handle, void *, char *,
                                 sds_code, sds_code, sds_code, sds_code);

#if defined(SDSDB)
EXTERN sds_handle sds_db_make(sds_handle,char *,char *,int,char **);
#endif

EXTERN sds_handle sds_define_structure(sds_handle,struct type_list *,char *);
EXTERN sds_handle sds_declare_structure(sds_handle, void *,
                                               char *,sds_code,sds_code);
EXTERN sds_handle     sds_write2fd(int , sds_handle );
EXTERN sds_handle     sds_fullsize(sds_handle);
EXTERN sds_handle     sds_read_open_fd(int,int);
EXTERN sds_handle     sds_dup(sds_handle, char *);
EXTERN sds_handle     sds_new(char *);
EXTERN sds_handle     sds_access(char *,sds_code, sds_code);
EXTERN sds_handle     sds_pmem_attach(char *);
EXTERN sds_handle     sds_ass(sds_handle,char *,sds_code);
EXTERN sds_handle     sds_file2shm(char *,char *);
EXTERN sds_handle     sds_name2ind(sds_handle ,char *);
EXTERN sds_handle     sds_like2ind(sds_handle ,char *, sds_handle);

#endif
