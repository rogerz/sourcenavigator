/* $Header$ */

#if !defined(sds_externs_h)
#define sds_externs_h 1

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

EXTERN void *         sds_malloc(sds_code );
EXTERN void *         sds_calloc(sds_code ,sds_code);
EXTERN void *         sds_realloc(void *, sds_code );
EXTERN sds_handle sds_type_duplicate_def(sds_handle,sds_handle,sds_code);

EXTERN sds_handle     sds_load_direc(sds_handle,int, struct sds_header *);
EXTERN struct  direc *f2mem(sds_handle, char *);
EXTERN void           set_sys_vars(sds_handle, struct direc *);
EXTERN int            sds_namelist(char *, char *, char);

EXTERN sds_handle     next_sds(void);

EXTERN void           nexpand(char *,char *);
EXTERN void          *sds_getp(struct direc *,sds_handle);
EXTERN void           fix_sizes_and_aligns(sds_handle);

EXTERN char          *get_heap(sds_handle);

EXTERN struct direc  *sds_direc_ptr(sds_handle);

EXTERN sds_handle     sds_tlist_add(sds_handle,struct type_list *);
EXTERN sds_handle     object_size(struct direc *, struct type_list *,char *);
EXTERN sds_handle     object_tlsize(rescon *,struct direc *,
                                  char *, char *, struct type_list *);

EXTERN sds_code       convert(sds_handle,int,struct sds_odesc*,struct sds_odesc*,int);

/*******       warning: error return is -1 ***************/
EXTERN sds_off_t      sds_add_to_heap(sds_handle,char *, char);
EXTERN char          *sds_jstr(char *, unsigned long);

EXTERN struct sds_header   *get_header(sds_handle);
EXTERN struct type_list    *get_tlist(sds_handle);
EXTERN struct direc        *header_to_direc(struct sds_header *);

EXTERN struct direc        *shm_attr(char *);
EXTERN struct direc        *shm_attw(char *);

EXTERN unsigned long  tlist_size(struct type_list *);
EXTERN unsigned long  sds_tlsize(sds_handle, long, char *);
EXTERN sds_handle     get_heap_size(sds_handle);
EXTERN sds_handle     offil(sds_handle, char **, int, off_t*);
EXTERN unsigned long  sds_sz(struct direc *);
EXTERN sds_handle     to_file(sds_handle, int, char **);
EXTERN sds_handle     sds_mem_header(sds_handle, char *);
EXTERN sds_code       align_delta(sds_off_t, char);
EXTERN int            sds_write(int ,char *,int );
EXTERN int            sds_buffered(int, char *, int, char *, int, int *);
EXTERN int            sds_flush(int, char *, int *);
EXTERN sds_handle     sds_write_header(int, sds_handle,char *,int,int *);
EXTERN sds_handle     sds_write_object(int, sds_handle, sds_code, void *,char *,int,int *);
EXTERN int            sds_write_data(int, char *, int,char *,int,int *);
EXTERN sds_handle     sds_write_pattern(int, unsigned long, char,char *,int,int *);
EXTERN sds_handle     sds_read_header(sds_handle,int, struct sds_header *);
EXTERN sds_handle     sds_vread(int, char *, int);
EXTERN int            sds_read_data(int, char *, int);
EXTERN sds_handle     sds_which(struct direc *);
EXTERN sds_handle     write_sds(int, sds_handle);

EXTERN off_t          fsiz(char *);

#endif

