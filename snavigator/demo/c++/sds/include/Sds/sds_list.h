#ifndef sds_list_h
#define sds_list_h 1

#include "Sds/sdsgen.h"



struct list_control;
struct node_control;

struct node_control {
  struct node_control *next;
  struct node_control *prev;

  struct list_control *up;
  struct list_control *down;
  char                *name;
  void                *data;
  long                 datasize;
  sds_handle           sds;
  sds_handle           object;
  sds_handle           datatype;
  sds_handle           nelems;
  char                 align;
  unsigned int         marked_for_delete :1 ;
  unsigned int         node_was_malloced :1 ;
  unsigned int         data_was_malloced :1 ;
};

struct list_control {
  struct node_control *head;
  struct node_control *tail;
  long                 nnodes;
  long                 listid;
  char                *name;

  struct node_control *current;
  void                *databank;
  long                 banksize;
  long                 bankused;
  sds_code             banktype;
  sds_handle           sds;
  sds_handle           object;
  sds_code             datatype;
  unsigned int         is_deleted      : 1;
  unsigned int         do_malloc       : 1;
  unsigned int         has_malloc      : 1;
  unsigned int         is_top          : 1;
  unsigned int         is_bottom       : 1;
  unsigned int         is_homogeneous  : 1;
  unsigned int         done_output     : 1;
};

typedef struct sds_listcon sds_listcon;

struct sds_listcon {
  struct list_control *lc;
  int lastlist;
  int listsdeleted;
  int nlists;
};

#if defined(__STDC__)
void                  sds_remove_data(struct node_control *);
struct node_control * sds_new_struct(int ,long,sds_handle, sds_handle);
struct node_control * sds_new_array(int ,long,long);
int                   sds_downlist(int , struct node_control *);
void                  dump(void);
int                   sds_new_list(char *, int );
int                   sds_give_databank(int , void *, long, sds_code);
int                   sds_homogeneous_struct(int ,sds_handle,sds_handle);
int                   sds_homogeneous_array(int ,sds_code);
int                   sds_make_as_list(char *,int , sds_handle, sds_handle);
void                  sds_save_lists(char *);

int                   sds_gobefore(struct list_control *,struct node_control *);
struct node_control * sds_next( struct list_control *, struct node_control *);
struct list_control * goodlist(int);
struct node_control * sds_add(struct list_control *,char *,void *,int,int,int);

#else
void                  sds_remove_data();
struct node_control * sds_new_struct();
struct node_control * sds_new_array();
int                   sds_downlist();
void                  dump();
int                   sds_new_list();
int                   sds_give_databank();
int                   sds_homogeneous_struct();
int                   sds_homogeneous_array();
int                   sds_make_as_list();
void                  sds_save_lists();

int                   sds_gobefore();
struct node_control * sds_next();
struct list_control * goodlist();
struct node_control * sds_add();


#endif

#endif
