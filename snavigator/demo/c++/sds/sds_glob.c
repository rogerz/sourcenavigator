
/* $Header$*/

#include "Sds/sdsgen.h"
#include "Sds/sds_errstrings.h"
#include "Sds/sds_externs.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#if !defined(VXWORKS)
#include <sys/time.h>
#endif
#include <sys/types.h>
#include <unistd.h>


#if defined(__SUN5)
#define OPEN_MAX FOPEN_MAX
#else
#if defined(__linux__)
#include <i386/fpu_control.h> /* See note in sds_global_init() below */
#include <linux/limits.h>
#else
#if defined(VXWORKS)
#include <types/vxParams.h>
#define OPEN_MAX _PARM_FOPEN_MAX
#else
#include <sys/param.h>
#define OPEN_MAX NOFILE
#endif
#endif
#endif

#if !defined(VXWORKS)
#if !defined(__MSDOS__) 
#include <malloc.h>
#include <memory.h>
#else
#include <alloc.h>
#include <mem.h>
#endif
#endif

/* This for backward compatibility with the old error system */
int sds_error;


/* forwards..... */
struct sds_control_p * sds_new_scp(VOIDDEF);
#if defined(__STDC__)
void printoutprog(int);
void printout(int);
#else
void printoutprog();
void printout();
#endif

extern void sds_cleanc(rescon *);

/* The sizes of all the primitive types on the current architecture, in
   bytes 
*/
static char sds_sizes[NPRIMITIVES] = 
{
    (char)0,                      /* struct */
    (char)sizeof(char),           /* pad - unused */
    (char)sizeof(char),           /* byte */
    (char)sizeof(char),           /* unsigned byte */
    (char)sizeof(short),          /* word */
    (char)sizeof(short),          /* unsigned word */
#if (SDS_BIGADDR == 1)          /* 8-byte address and long */
    (char)sizeof(int),            /* 4-byte integer */
    (char)sizeof(int),            /* unsigned 4-byte integer */
#else                             /* 4-byte address and long */
    (char)sizeof(long),           /* 4-byte integer */
    (char)sizeof(long),           /* unsigned 4-byte integer */
#endif
    (char)sizeof(float),          /* float */
    (char)sizeof(double),         /* double */
    (char)sizeof(float),          /* vax float */
    (char)sizeof(double),         /* vax D-double */
    (char)sizeof(double),         /* vax G-double */
    (char)1,                      /* C string - zero terminated */
    (char)sizeof(struct direc),   /* direc structure */
    (char)2*sizeof(float),        /* complex */
    (char)2*sizeof(double),       /* double complex */
    (char)2*sizeof(float),        /* vax complex */
    (char)2*sizeof(double),       /* vax D-double complex */
    (char)2*sizeof(double),       /* vax G-double complex */
    (char)sizeof(char),           /* logical byte */
    (char)sizeof(short),          /* logical word */
#if (SDS_BIGADDR == 1)          /* 8-byte address and long */
    (char)sizeof(int),            /* logical 4-byte */
#else                           
    (char)sizeof(long),           /* logical 4-byte */
#endif
    (char)sizeof(char*),          /* pointer */
    (char)2*sizeof(long),         /* time (eg sybase time ) */
    (char)0,                      /* Sds */
    (char)1,                      /* fixed length string, no terminator */
#if (SDS_BIGADDR == 1)          /* 8-byte address and long */
    (char)sizeof(int),            /* size modifier */
#else                           
    (char)sizeof(long),           /* size modifier */
#endif
    (char)sizeof(void*),          /* Glish value */
#if (SDS_BIGADDR == 1)          /* 8-byte address and long */
    (char)sizeof(int),            /* Unix time */
#else                           
    (char)sizeof(long),           /* Unix time */
#endif
    (char)0,                      /* bitfield */
#if (SDS_BIGADDR == 1)          /* 8-byte address and long */
    (char)sizeof(long),           /* internal pointer */
    (char)sizeof(long),           /* 8-byte integer */
#else                           
    (char)sizeof(int),            /* internal pointer */
    (char)2*sizeof(long),         /* 8-byte integer */
#endif
    (char)0,                      /* End bitfield */
    (char)sizeof(char),           /* byte bitfield */
    (char)sizeof(short),          /* word bitfield */
#if (SDS_BIGADDR == 1)          /* 8-byte address and long */
    (char)sizeof(int),            /* 4-byte bitfield */
    (char)sizeof(long),           /* 8-byte bitfield */
    (char)sizeof(long),           /* 8-byte unsigned long */
#else                           
    (char)sizeof(long),           /* 4-byte bitfield */
    (char)2*sizeof(long),         /* 8-byte bitfield */
    (char)2*sizeof(long),         /* 8-byte unsigned long */
#endif
    };

/* ...and the corresponding names */

static char type_name[NTYPES + 1][22] = {
    "Structure",              
    "Pad", 
    "Byte", 
    "Uns Byte",
    "Word",
    "Uns Word",
    "Long32",
    "Uns Long32",
    "Float",
    "Double",
    "Vax Float",
    "Vax D-Double",
    "Vax G-Double",
    "C-String",
    "Direc",
    "Complex",
    "Complex Double",
    "Vax Complex",
    "Vax Complex D-Double",
    "Vax Complex G-Double",
    "Logical Byte",
    "Logical Word",
    "Logical Long32",
    "Pointer",
    "Time",
    "Sds",
    "F-String",
    "Size Modifier",
    "Glish Value",
    "Unix Time",
    "Bitfield",
    "Internal Pointer",
    "Long64",
    "End Bitfield",
    "Char Bitfield",
    "Short Bitfield",
    "Long32 Bitfield",
    "Long64 Bitfield",
    "UnsLong64",
    "Unknown Type"
    };

static char c_type_name[NTYPES + 1][22] = {
    "struct",              
    "char", 
    "char", 
    "unsigned char",
    "short",
    "unsigned short",
    "long",
    "unsigned long",
    "float",
    "double",
    "float",
    "double",
    "double",
    "char",
    "struct direc",
    "Complex",
    "Complex Double",
    "Vax Complex",
    "Vax Complex D-Double",
    "Vax Complex G-Double",
    "unsigned char",
    "unsigned short",
    "unsigned long",
    "char *",
    "time_t",
    "Sds",
    "char",
    "Size Modifier",
    "Glish Value",
    "long",
    "Bitfield",
    "Internal Pointer",
    "long long",
    "End Bitfield",
    "Char Bitfield",
    "Short Bitfield",
    "Long32 Bitfield",
    "Long64 Bitfield",
    "UnsLong64",
    "Unknown Type"
    };

char *
sds_typename(code)
sds_code(code);
/* return the name of a given primitive type code, or 'Unknown Type' if 
   the thing is complex */
{ return code > NPRIMITIVES?type_name[NPRIMITIVES]:type_name[code]; }

char *
sds_c_typename(code)
sds_code(code);
{ return code > NPRIMITIVES?c_type_name[NPRIMITIVES]:c_type_name[code]; }

char 
sds_psize(code)
sds_code code;
/* return the primitive size of a given type code, or 0 if the thing is 
  complex */
{ return code > NPRIMITIVES?(char)0:sds_sizes[code]; }

/* See sds_mdep.h: most restrictive byte boundary for each architecture*/
static char  sds_rbytes[NARCS] = { 2,8,1,8,4};

/* The following gives alignment for different SDS_XXX */
/* Note that there is a special code for SDS_BITFIELD whose alignment
   is only there to pad the array correctly - its alignment depends
   on where it is in its integer or short or whatever. SDS_BITFIELD
   types modify whatever integer type they follow. Here their 
   alignment is marked '0'.
 */
static char  sds_arcs[NARCS][NPRIMITIVES] = 
         {
          {2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, /* 680x0 */
           1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 1,
           1, 2, 2, 2, 0, 2, 2, 0, 1, 2, 2, 2, 2},
          {8, 1, 1, 1, 2, 2, 4, 4, 4, 8, 4, 8, 8, /* SUN, HP MIPS RISC */
           1, 4, 4, 8, 4, 8, 8, 1, 2, 4, 4, 4, 1,
           1, 4, 4, 4, 0, 4, 4, 0, 1, 2, 4, 4, 4},
          {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /* VAX VMS */
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1},
          {8, 1, 1, 1, 2, 2, 4, 4, 4, 8, 4, 8, 8, /* Alpha.8byte long,addr */
           1, 8, 4, 8, 4, 8, 8, 1, 2, 4, 8, 4, 1,
           1, 4, 8, 4, 0, 8, 8, 0, 1, 2, 4, 8, 8},
          {4, 1, 1, 1, 2, 2, 4, 4, 4, 4, 4, 4, 4, /* ultrix, hypercube */
           1, 4, 4, 4, 4, 4, 4, 1, 2, 4, 4, 4, 1,
           1, 4, 4, 4, 0, 4, 4, 0, 1, 2, 4, 4, 4}
           };

/* The single pointer through which all SDS globals are referenced */
static struct sds_sys_control *ssc = NULL;

/* The following calls return information about the current, or other,
   architectures; they are here so that the data is not directly accessed
*/
char 
sds_palign(code)
sds_code code;
{ return code > NPRIMITIVES?(char)0:sds_arcs[SDS_ARC][code]; }

char 
sds_arc_palign(arc, code)
int arc;
sds_code code;
{
  return arc < 0 || arc > NARCS || code > NPRIMITIVES?
    (char)0:sds_arcs[arc][code];
}

char 
sds_arc_rbyte(arc)
int arc;
{  return sds_rbytes[arc]; }

char 
sds_rbyte(VOIDDEF)
{  return sds_rbytes[SDS_ARC]; }

char *sds_arc_aligns(arc)
int arc;
{ return sds_arcs[arc]; }

/* end of architecture information calls */

/* Check to see if an sds handle refers to a valid dataset.
   Return 1 for a good handle referring to a real dataset.
   Return 0 and set error to SDS_NO_SUCH_SDS if handle is out-of-range.
   Return 0, do not set error if handle is usable (ie in range but
             not referring to a real dataset).
*/
int
sds_dataset_check(sds)
sds_handle sds;
{
  if (!ssc)
  {
    fprintf(stderr,"Sds must be initialised with sds_init()");
    exit(1);
  }
  if ((sds > sds_max()) || (sds <= 0L))
  {
    sds_push_error(SDS_NO_SUCH_SDS, SDS_RETURN,
              "Invalid sds handle when accessing system tables");
    return 0;
  }
  if (!ssc->sds_cp[sds-1])
    return 0;
  return 1;
}

/* The following calls return control structures for global SDS
   manipulations.
*/
struct sds_control_p *
sds_control(sds)
sds_handle sds;
{ 
#if !defined(SDS_GO_FAST)
  return sds_dataset_check(sds)?ssc->sds_cp[(int)sds - 1]:
            (struct sds_control_p *)0;
#else
  return ssc->sds_cp[(int)sds - 1];
#endif
}

struct sds_saverestore *
sds_saver(VOIDDEF)
{ return &ssc->sr; }

int 
sds_maxbufsize(VOIDDEF)
{ return ssc->maxbufsize; }

void 
sds_setmaxbuf(int size)
{ ssc->maxbufsize = size; }

int 
sds_fd_buffer(VOIDDEF)
{ return ssc->fd_buffer; }

void 
sds_setfd_buffer(int fd_buffer)
{ ssc->fd_buffer = fd_buffer; }

int
sds_initialised(VOIDDEF)
{ return ssc == 0?0:1; }

int
sds_initialise_enabled(VOIDDEF)
{ return ssc?ssc->re_init:1; }

void 
sds_reinit_enable(VOIDDEF)
{ if (ssc) ssc->re_init = 1; }

int
sds_genarc(sds)
sds_handle sds;
#if !defined(SDS_GO_FAST)
{ return sds_dataset_check(sds)?(int)ssc->sds_cp[(int)sds - 1]->genarc:-1; }
#else
{ return (int)ssc->sds_cp[(int)sds - 1]->genarc; }
#endif

int
sds_stream(sds)
sds_handle sds;
#if !defined(SDS_GO_FAST)
{ return sds_dataset_check(sds)?ssc->sds_cp[(int)sds - 1]->stream:0; }
#else
{ return ssc->sds_cp[(int)sds - 1]->stream; }
#endif

int
sds_source(sds)
sds_handle sds;
#if !defined(SDS_GO_FAST)
{ return sds_dataset_check(sds)?ssc->sds_cp[(int)sds - 1]->source:0; }
#else
{ return ssc->sds_cp[(int)sds - 1]->source; }
#endif

int
sds_heap_size(sds)
sds_handle sds;
{ 
  int size, psize;
  if (!sds_dataset_check(sds))
    return 0;
  size = ssc->sds_cp[(int)sds - 1]->current_heap - ssc->sds_cp[(int)sds -1]->heap;
  psize = align_delta(size,4);
  if (psize)
    memset(ssc->sds_cp[(int)sds - 1]->current_heap, 0, psize);
  return size + psize;
}

int
sds_direc_size(sds)
sds_handle sds;
{ return sds_dataset_check(sds)?ssc->sds_cp[(int)sds - 1]->direc_size:0; }

int **
sds_varel(sds)
sds_handle sds;
{
  return sds_dataset_check(sds)?ssc->sds_cp[(int)sds-1]->varel_count:0; 
}

sds_handle *
sds_dup_size(sds)
sds_handle sds;
{
  return sds_dataset_check(sds)?ssc->sds_cp[(int)sds - 1]->dup_size:0; 
}

char *
sds_loadname(sds)
sds_handle sds;
{
  return sds_dataset_check(sds)?ssc->sds_cp[(int)sds - 1]->load_name:0;
}

void
sds_trap_bigint_overflow(int tf)
{ 
  if (tf) ssc->controlbits |= SDS_TRAP_DOUBLE_LONG_OVERFLOW;
  else ssc->controlbits &= ~SDS_TRAP_DOUBLE_LONG_OVERFLOW;
}

void
sds_bigint_trap(void *generalpointer)
{
  sds_perror("Conversion overflow 64-bit integer to 32-bit integer");
}

void
sds_truncate_bigints()
{ 
  ssc->controlbits &= ~SDS_DOUBLE_LONG_MASK;
  ssc->controlbits |= SDS_TRUNCATE_DOUBLE_LONGS;
}

void
sds_float_bigints()
{ 
  ssc->controlbits &= ~SDS_DOUBLE_LONG_MASK;
  ssc->controlbits |= SDS_FLOAT_DOUBLE_LONGS;
}

void
sds_pack_bigints()
{ 
  ssc->controlbits &= ~SDS_DOUBLE_LONG_MASK;
  ssc->controlbits |= SDS_DOUBLE_LONGS_IN_LONGS;
}

int sds_query_truncate_b(void) { return ssc->controlbits & SDS_TRUNCATE_DOUBLE_LONGS;}
int sds_query_trap_b(void) { return ssc->controlbits & SDS_TRAP_DOUBLE_LONG_OVERFLOW;}
int sds_query_float_b(void) { return ssc->controlbits & SDS_FLOAT_DOUBLE_LONGS;}
int sds_query_pack_b(void) { return ssc->controlbits & SDS_DOUBLE_LONGS_IN_LONGS;}

char *
sds_heap(sds)
sds_handle sds;
{
#if !defined(SDS_GO_FAST)
  return sds_dataset_check(sds)?ssc->sds_cp[(int)sds - 1]->heap:0; 
#else
  return ssc->sds_cp[(int)sds - 1]->heap; 
#endif
}

char *
sds_current_heap(sds)
sds_handle sds;
{
  return sds_dataset_check(sds)?ssc->sds_cp[(int)sds-1]->current_heap:0;
}

char ***
sds_estart(sds)
sds_handle sds;
{
  return sds_dataset_check(sds)?ssc->sds_cp[(int)sds - 1]->element_start:0;
}

rescon *
sds_new_rescon()
{
  rescon *c = (rescon *)sds_calloc(1,sizeof(rescon));
  c->stack_size = 0;
  c->start_stack = 0;
  sds_cleanc(c);
  return c;
}

void 
sds_delete_rescon(rescon *c)
{
  if (c)
  {
    sds_cleanc(c);
    free((char *)c);
  }
}

struct sds_res_control *
sds_src(sds)
sds_handle sds;
{
  if (!sds_dataset_check(sds))
    return 0;
  if (!(ssc->sds_cp[(int)sds - 1]->src))
  {
    ssc->sds_cp[(int)sds - 1]->src = sds_new_rescon();
  }
  return ssc->sds_cp[(int)sds - 1]->src;
}

/***********************************************************************/
void
sds_cleanup(sds_handle sds)
/***********************************************************************/
{
  struct sds_res_control *c = sds_src(sds);
  if (c) 
    sds_cleanc(c);
  else
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Initialise res stack with sds_rinit()");
  return;
}

struct sds_header *
sds_head(sds_handle sds)
{
#if !defined(SDS_GO_FAST)
  return sds_dataset_check(sds)?ssc->sds_cp[(int)sds - 1]->shead:0; 
#else
  return ssc->sds_cp[(int)sds - 1]->shead;
#endif
}

int
sds_set_riscpad(sds_handle sds)
{ 
  struct sds_header *h = sds_head(sds);
  if (h)
    h->controlbits |= SDS_IS_RISCY;
  return h?1:0;
}

struct direc *
sds_direc(sds)
sds_handle sds;
{
  return sds_dataset_check(sds)?ssc->sds_cp[(int)sds - 1]->dptr:0; 
}

/* for backward compatibility with V1 */
struct direc *
sds_direc_ptr(sds)
sds_handle sds;
{
  return sds_dataset_check(sds)?ssc->sds_cp[(int)sds - 1]->dptr:0; 
}

struct type_list *
sds_tlist(sds)
sds_handle sds;
{
#if !defined(SDS_GO_FAST)
  return sds_dataset_check(sds)?ssc->sds_cp[(int)sds - 1]->tlist:0; 
#else
  return ssc->sds_cp[(int)sds - 1]->tlist;
#endif
}

short
sds_alloflag(sds)
sds_handle sds;
{
  return sds_dataset_check(sds)?ssc->sds_cp[(int)sds - 1]->allofl:0;
}

/*********************************************************************/
void
sds_init(VOIDDEF)
/*********************************************************************/
{
  sds_global_init(MAX_SDS,ERRSTACK);
}

void
sds_global_init(maxsds,errstack)
int maxsds,errstack;
{
  int  i;
  struct tes {
    char a;
    double b;
  };
  if (sds_initialised() && !sds_initialise_enabled()) 
    return;
  if (!ssc)
  {
    ssc = (struct sds_sys_control *)malloc(sizeof(struct sds_sys_control));
    ssc->controlbits = SDS_DOUBLE_LONGS_IN_LONGS;
    ssc->fd_buffer = 0;
    ssc->sds_cp = 
      (struct sds_control_p **)malloc(maxsds * sizeof(struct sds_control_p *));
    ssc->maxsds = maxsds;
    ssc->maxbufsize = SDS_DEFAULT_BUFSIZE;
    ssc->sec = 
      (struct sds_error_control *)malloc(sizeof(struct sds_error_control));
    ssc->sec->errstack = errstack;
    ssc->sec->se = (struct sds_err *)malloc(sizeof(struct sds_err) * errstack);
  }
  ssc->re_init = 0;
  if ((sizeof(struct tes) == 9 && (SDS_ARC != SDS_VAXARC)) ||
      (sizeof(struct tes) == 10 && (SDS_ARC != SDS_SUN3ARC)))
  {
    fprintf(stderr, "Sds is using the wrong architecture!\n");
    fprintf(stderr, "At present the only know way of this happening is if you \n");
    fprintf(stderr,"are running on a DOS machine and have used the \n");
    fprintf(stderr,"alignment switches on the complier and have not\n");
    fprintf(stderr,"told sds; eg if the -a switch is used on the TurboC\n");
    fprintf(stderr,"compiler you must also define TURBOWORD\n");
    exit(1);
  }
#if defined(__linux__) /* WHY? For Pete's sake?
                      Well, the default Linux exception handling is not
                      ieee, so one sets it or lots of
                      things will die.  Commaster also does this...  */
  __setfpucw(_FPU_IEEE);
#endif

  for (i=0;i<sds_max();i++) 
    ssc->sds_cp[i] = NULL;

    /******  Error stack initialisation *****/
/* This is where I am in the stack: -1 means no
   errors so the first push will take it to 0
 */
  ssc->sec->stack_level = -1;

/* if a FATAL error is encountered, should the system exit? */
  ssc->sec->exit_on_fatal = 1;
  ssc->sec->stack_overwritten = 0;

/* Do I immediately output error reports ( >0 ) or not? */
  ssc->sec->output_level = SDS_BE_QUIET;

  ssc->sec->proginfo = 0;
}

int sds_max(VOIDDEF)
{ return ssc->maxsds; }
int sds_errstack()
{ return ssc->sec->errstack; }

/*********************************************************************/
sds_handle
next_sds(VOIDDEF)
/*********************************************************************/
{
  sds_handle sds = 0L;

  while (ssc->sds_cp[sds] != NULL)
  {
    if (sds == sds_max() - 1L)
    {
      sds_push_error(SDS_NO_SPC,SDS_ERROR,
                      "Searching for free system table space");
      return 0L;
    }
    sds++;
  }
  ssc->sds_cp[sds] = sds_new_scp();
  return sds + 1L;
}

/* Make and initialise a new control structure */
struct sds_control_p *
sds_new_scp(VOIDDEF)
{
  struct sds_control_p *scp = 
            (struct sds_control_p *)malloc(sizeof(struct sds_control_p));
  scp->allofl = (short)0;
  scp->stream = SDS_FILE_OP;
  scp->source = 0;
  scp->heap_size = 0;
  scp->direc_size = 0;
  scp->tree = 0;
  scp->file_offsets = (sds_handle *)0;
  scp->varel_count = (int **)0;
  scp->dup_size = 0L;
  scp->load_name = (char *)0;
  scp->target_name = (char *)0;
  scp->heap = (char *)0;
  scp->current_heap = (char *)0;
  scp->element_start = (char ***)0;
  scp->shead = (struct sds_header *)0;
  scp->tlist = (struct type_list *)0;
  scp->dptr = (struct direc *)0;
  scp->src = 0;
  scp->genarc = SDS_ARC; /*Lets assume it's native until we discover otherwise*/
  scp->is_proto = 1;
  return scp;
}

/* ...and delete everything (Stuff inside already deleted) */
void
sds_delete_scp(scp)
struct sds_control_p *scp;
{
  char *tmp;
  int sds = 0L;
  while (ssc->sds_cp[sds++] != scp)
    if (sds == sds_max())
      return;
  sds--;
  tmp = (char *)scp;
  ssc->sds_cp[sds] = NULL;
  free(tmp);
}
/*********************************************************************/
sds_handle
good_sds(name)
char  *name;
/*********************************************************************/
{
  int sds;

  if (!sds_initialised())
  {
    fprintf(stderr,"Sds must be initialised with sds_init()");
    exit(1);
  }
  else if (strcmp(name,"")) 
  {
    for (sds = 0;sds < sds_max(); sds++)
    {
      if (sds_control(sds))
        if (sds_control(sds)->dptr != NULL && 
            !strcmp(name,sds_obind2name(sds,0L)))
          return (sds_handle)sds;
    }
  }
  return 0L;
}

/************* Error stack code *******************************************/
void
shiftdown(VOIDDEF)
{
  int i;
  ssc->sec->stack_overwritten = 1;
  for (i=1; i < sds_errstack(); i++)
    memcpy((char *)(&ssc->sec->se[i - 1]), 
           (char *)(&ssc->sec->se[i]), 
           sizeof(struct sds_err));
  return;
}

/**************************************************************************/
/* Print out program info for the i'th entry on the stack */
void
printoutprog(i)
int i;
{
  if (ssc->sec->se[i].line != -1)
  {
    fprintf(stderr, "   From file %s line %d:\n",
    ssc->sec->se[i].filename,
     ssc->sec->se[i].line);
    if (errno != 0)
    {
      perror("Last system error");
      errno = 0;
    }
  }
  return;
}
/**************************************************************************/
/* Print out the i'th entry on the stack */
void
printout(i)
int i;
{
  if (i <= ssc->sec->stack_level)
  {
    if (ssc->sec->proginfo)
      printoutprog(i);

    fprintf(stderr,"%d: %s: %s (%s)\n",
       i,
       sds_error_levels[ssc->sec->se[i].errlevel],
       ssc->sec->se[i].errstring,
       sds_error_string[-ssc->sec->se[i].errcode]);

    if (ssc->sec->exit_on_fatal && ssc->sec->se[i].errlevel == SDS_FATAL)
    {
      fprintf(stderr, "Error reporting instructs exit\n");
      exit(ssc->sec->exit_on_fatal & 0xff);
    }
  }
  return;
}

/**************************************************************************/
/* Push a new error onto the stack. We assume that errstring is zero
   terminated, but check to see if it is NULL
*/
int
sds_push_lerror(errcode, errlevel, errstring, line, filename)
int errcode,errlevel;
char *errstring;
int line;
char *filename;
{
  /* A couple of safties first */
  char temp = 0;
  if (errstring == (char *)0) /* Just in case */
    errstring = &temp;

  if (errlevel >= SDS_ERROR)
    sds_error = errcode; /* for backward compatibility */
  if (-errcode > SDS_MAX_ERR) /* This makes no sense for Sds */
  {
    errcode = SDS_BAD_ERROR_REGISTERED;
    errlevel = SDS_FATAL;
  }

  /* If I'm at the end of the stack, I shift every entry down, thus loosing
     the deepest one. Otherwise I can increment the stack level 
   */
  if (ssc->sec->stack_level == sds_errstack() - 1)
    shiftdown();
  else
    ssc->sec->stack_level++;

  /* OK, load the stack */
  ssc->sec->se[ssc->sec->stack_level].errcode = errcode;
  ssc->sec->se[ssc->sec->stack_level].errlevel = errlevel;
  ssc->sec->se[ssc->sec->stack_level].line = line;
  strncpy(ssc->sec->se[ssc->sec->stack_level].errstring, errstring, ERRSTRINGLEN - 1);
  ssc->sec->se[ssc->sec->stack_level].errstring[ERRSTRINGLEN-1] = 0;
  strncpy(ssc->sec->se[ssc->sec->stack_level].filename, filename,ERRSTRINGLEN - 1);
  ssc->sec->se[ssc->sec->stack_level].filename[ERRSTRINGLEN-1] = 0;

  /* Verbose or not ? */
  if (ssc->sec->output_level != SDS_BE_QUIET && errlevel >= ssc->sec->output_level)
    printout(ssc->sec->stack_level);

  /* And I say how deep the stack is now */
  return ssc->sec->stack_level;
}

/**************************************************************************/
/* To put a user mark in the program */
int
sds_lmark(comment, line, filename)
char *comment;
int line;
char *filename;
{ return sds_push_lerror(SDS_USER_MARK, 0, comment, line, filename); }

/**************************************************************************/
int 
sds_last_error(VOIDDEF)
{
 int i;
 if (ssc->sec->stack_level != -1)
 {
   for (i=ssc->sec->stack_level; i >= 0; i--)
   if (ssc->sec->se[i].errlevel == SDS_ERROR)
     return ssc->sec->se[i].errcode;
 }
 return 0;
}

/**************************************************************************/
int 
sds_last_warning(VOIDDEF)
{
 int i;
 if (ssc->sec->stack_level != -1)
 {
   for (i=ssc->sec->stack_level; i >= 0; i--)
   if (ssc->sec->se[i].errlevel == SDS_WARNING)
     return ssc->sec->se[i].errcode;
 }
 return 0;
}

/**************************************************************************/
int 
sds_last_return(VOIDDEF)
{ 
 if (ssc->sec->stack_level != -1)
   return ssc->sec->se[ssc->sec->stack_level].errcode;
 return 0;
}


/**************************************************************************/
int
sds_pop_error(errlevel, errstring)
int *errlevel;
char **errstring;
{
  int tempcode;
  if (ssc->sec->stack_level == -1) /* No errors */
    return 0;
  *errlevel = ssc->sec->se[ssc->sec->stack_level].errlevel;
  tempcode = ssc->sec->se[ssc->sec->stack_level].errcode;
  *errstring = ssc->sec->se[ssc->sec->stack_level].errstring;
  ssc->sec->stack_level--;
  return tempcode;
}

/**************************************************************************/
void 
sds_output_proginfo(truefalse)
int truefalse;
{ ssc->sec->proginfo = truefalse; }

/**************************************************************************/
void 
sds_exit_on_fatal(truefalse)
int truefalse;
{ ssc->sec->exit_on_fatal = truefalse; }

/**************************************************************************/
void 
sds_clear_errors(VOIDDEF)
{
  ssc->sec->stack_level = -1;
  ssc->sec->stack_overwritten = 0;
  sds_error = 0;
  return;
}

/**************************************************************************/
void 
sds_output_errors(level)
int level;
{ ssc->sec->output_level = level; }

/**************************************************************************/
void 
sds_perror(comment)
char *comment;
{
  int i;
  if (ssc->sec->stack_overwritten)
    fprintf(stderr,"Warning: the stack grew too big, some errors lost\n\n");

  fprintf(stderr,"\n%s", comment);

  if (ssc->sec->stack_level != -1)
  {
    fprintf(stderr,"\nError stack, last error first:\n");
    for (i=sds_errstack() -1 ;i >= 0;i--)
      printout(i);
  }
  else
  {
    fprintf(stderr, "\nSds error stack is clear\n");
  }

  if (errno != 0)
  {
    fprintf(stderr,"******** ********\n");
    perror("Last system error");
  }
  return;
}
/**************************************************************************/
void
sds_stop_if_error(comment)
char *comment;
{
  if (sds_last_error())
  {
    sds_exit_on_fatal(0);
    sds_output_proginfo(1);
    sds_perror(comment);
    exit(1);
  }
  return;
}
