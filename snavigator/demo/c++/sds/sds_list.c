/* $Header$ */

#include "Sds/sdsgen.h"
#include "Sds/sds_lisd.h"
#include "Sds/sds_externs.h"
#include <string.h>

#if defined (__MSDOS__)
#include <alloc.h>
#else
#include <stdlib.h>
#endif

#define MBLOCK 16

static int listindent = -1;
static long listsleft = 0;

/*   forward declarations */
void                  dump_list(struct list_control *);
int                   recoverlist(void);
void                  list_init(struct list_control *, char *, int);
int                   sds_add_node(struct list_control *, 
                                   struct node_control *);
void                  sds_init_node(struct node_control *);
int                   sds_delete_node(struct node_control *);
void                  dumpnode(struct node_control *, void *);
void                  sds_bank_compress(int);
int                   sds_homogeneous_data(int,sds_code,sds_handle,sds_handle);
struct list_control * goodlist(int);
struct node_control * sds_new_node(void);
struct node_control * sds_new_data(int,long,long,sds_handle,sds_handle,void *);
int                   sds_allnodes(void);
void                  sds_concat_nodes(sds_handle, struct node_control *,
                         struct list_control *, long *, long *);
int                   sds_concat_list(sds_handle,struct node_control *,
                         struct list_control *,long *, long *, long);
extern void pindent(int);


/******************************************************************
     EXPORT Level 1
long sds_new_list(name, id)
   Create a new list, with a name and id supplied by the user.
   Space for the list is reclaimed from any deleted list, from the
   already allocated array, or from new allocations if necessary.
   Malloc failures result in exit, otherwise no error return.
******************************************************************/
int
sds_new_list(name, id)
char *name;
int id;
{
  sds_listcon *slc = sds_listc();
  slc->lastlist++;
  if (slc->listsdeleted != 0) /* There are some list(s) marked 'deleted' */
  {
    long rlist;
    if ((rlist = recoverlist()) >= 0)
    {
      list_init(&slc->lc[rlist],name, id);
      return rlist;
    }
  }
  if (listsleft == 0) /* I have to allocate some more space */
  {
    slc->nlists += MBLOCK;
    listsleft = MBLOCK;
    if (slc->lc == NULL)
      slc->lc = (struct list_control *)sds_malloc(slc->nlists * sizeof(struct list_control));
    else
      slc->lc = (struct list_control *)sds_realloc((char *)slc->lc, slc->nlists * sizeof(struct list_control));
  }
  listsleft--;
  list_init(&slc->lc[slc->lastlist - 1], name, id);
  return slc->lastlist;
}

/******************************************************************
     EXPORT Level 1
int sds_homogeneous_struct(list_index, sds, object)
int sds_homogeneous_array(list_index, type )
     INTERNAL
int sds_homogeneous_data(list_index, datatype, sds, object)
  These three functions declare that all the data objects in a list
  will be of the same type - either primitive (sds_homogeneous_array)
  or compound (sds_homogeneous_struct()). Multiplicities are given 
  when individual nodes in the list are declared. Both these user
  functions use sds_homogeneous_data() for implementation.
******************************************************************/
int
sds_homogeneous_struct(list_index, sds, object)
int list_index;
sds_handle sds,object;
{  return sds_homogeneous_data(list_index,0, sds, object); }

int
sds_homogeneous_array(list_index, type )
int list_index;
sds_code type;
{ return sds_homogeneous_data(list_index,type, SDS_NO_SUCH_SDS, 0); }

int
sds_homogeneous_data(list_index, datatype, sds, object)
int list_index;
sds_handle sds,object;
sds_code datatype;
{
  struct list_control *list;
  if ((list = goodlist(list_index)) == NULL)
    return 0;
  list->sds = sds;
  list->object = object;
  list->datatype = sds_obind2code(sds, object);
  list->is_homogeneous = 1;
  return 1;
}

/******************************************************************
     EXPORT Level 1
int sds_give_databank(list_index, pointer, size, type)
  Instead of malloc()ing data space for new nodes, use a 
  pre-allocated block of data as a bank. This is mainly to
  load Fortran commons.
******************************************************************/
int
sds_give_databank(list_index, pointer, size, type)
int list_index;
void *pointer;
long size;
sds_code type;
{
  struct list_control *list;
  if ((list = goodlist(list_index)) == NULL)
    return 0;
  list->banktype = type;
  list->databank = pointer;
  list->banksize = size * (long)sds_psize(type);
  list->bankused = 0;
  list->do_malloc = 0;
  return 1;
}

/******************************************************************
     INTERNAL
void list_init(list, name, id)
  Initialise a new list with defaults.
******************************************************************/
void 
list_init(list, name, id)
struct list_control *list;
char *name;
int id;
{
  list->head = (struct node_control *)0;
  list->tail = (struct node_control *)0;
  list->nnodes  = 0;
  list->listid =id;
  list->name = (char *)malloc(strlen(name) + 1);
  strcpy(list->name, name);

  list->current = (struct node_control *)0;
  list->databank = (void *)0;
  list->banksize = 0L;
  list->bankused = 0L;
  list->banktype = 0L;
  list->sds = 0L;
  list->object = 0L;
  list->datatype = 0L;
  list->is_deleted  = 0;
  list->do_malloc  = 1;
  list->has_malloc  = 0;
  list->is_top  = 0;
  list->is_bottom  = 0;
  list->is_homogeneous  = 0;
  list->done_output  = 0;
}

/******************************************************************
     EXPORT Level 1
int sds_delete_list(list_index)
  Delete an existing list or warn that it doesn't exist.
******************************************************************/
int 
sds_delete_list(list_index)
int list_index;
{
  sds_listcon *slc = sds_listc();
  struct list_control *list;
  struct node_control *this;
  struct node_control *next;
  if ((list = goodlist(list_index)) == NULL)
    return 1;
  this = list->head;
  while (this != NULL)
  {  
    next = this->next;
    sds_delete_node(next);
    this = next;
  }
  list->is_deleted = 1;
  slc->listsdeleted++;
  return 1;
}

/******************************************************************
     INTERNAL
struct list_control * goodlist(list_index)
  Checks if the list_index is OK.
******************************************************************/
struct list_control *
goodlist(list_index)
int list_index;
{
  sds_listcon *slc = sds_listc();
  if (list_index < 1 || list_index > slc->lastlist )
  {
    sds_push_error(SDS_NO_SUCH_LIST, SDS_ERROR, "List index out-of-bounds");
    return (struct list_control *)0;
  }
  if (slc->lc[list_index - 1].is_deleted)
  {
    sds_push_error(SDS_NO_SUCH_LIST, SDS_ERROR, "List index deleted");
    return (struct list_control *)0;
  }
  return &slc->lc[list_index - 1];
}

/******************************************************************
     INTERNAL
int recoverlist(void)
  Look for a list marked 'deleted' so it can be re-used.
******************************************************************/
int
recoverlist(VOIDDEF)
{
  int i = 0;
  sds_listcon *slc = sds_listc();
  while(i < slc->nlists)
  {
    if (slc->lc[i].is_deleted)
    {
      slc->listsdeleted--;
      slc->lc[i].is_deleted = 0;
      return (long)i;
    }
    i++;
  }
  return 0;
}

/******************************************************************
     INTERNAL
void clean_lists(void)
  Cleans up the 'you've touched me' flags in the lists so that
  you can see if a list has already been treated when you traverse
  a tree of them.
******************************************************************/
void
clean_lists(VOIDDEF)
{
  int i;
  sds_listcon *slc = sds_listc();
  for (i = 0;i < slc->lastlist; i++)
    slc->lc[i].done_output = 0;
}

/******************************************************************
     EXPORT Level 2
void dump(void)
  Print out all lists and their nodes.
******************************************************************/
void
dump(VOIDDEF)
{
  int i;
  sds_listcon *slc = sds_listc();
  clean_lists();
  printf("----Dump lists and content definitions----\n");
  for (i = 0;i < slc->lastlist; i++)
  {
    if (!slc->lc[i].done_output)
      dump_list(&slc->lc[i]);
  }
  clean_lists();
}

/******************************************************************
     EXPORT Level 2
void dump_list(list)
  Print out this list and its nodes.
******************************************************************/
void
dump_list(list)
struct list_control *list;
{
  struct node_control *node;
  listindent++;
  pindent(listindent);
  if (list->is_deleted == 1)
    printf("Deleted ");
  printf("List start:id %ld: %s\n",list->listid, list->name);

  list->done_output = 1;

  if (list->databank != NULL)
  {
    pindent(listindent);
    printf("Databank at %lx, length %ld, %ld used\n",
             (unsigned long)list->databank,
             list->banksize,
             list->bankused);
  }
  node = list->head;
  if (list->is_homogeneous)
  {
    pindent(listindent);
    printf("Homogeneous list: each of %ld nodes controls:\n",list->nnodes);
    if (node->sds > 0)
      sds_list(node->sds,node->object,SDS_HEADER_ONLY); 
  }
  else
  {
    while(node != NULL)
    {
      dumpnode(node, list->databank);
      if (node->down != NULL)
        dump_list(node->down);
      node = node->next;
    }
  }
  pindent(listindent);
  printf("List end  :id %ld: %s\n",list->listid, list->name);
  listindent--;
}

/******************************************************************
     EXPORT Level 2
void dumpnode(node, base)
  Print out an node.
******************************************************************/
void
dumpnode(node, base)
struct node_control *node;
void *base;
{
  if (node->marked_for_delete)
  {
    pindent(listindent);
    printf("Deleted->");
  }
  if (node->sds != 0)
    sds_list(node->sds,node->object,SDS_HEADER_ONLY); 
  else
  if (base == NULL)
  {
    pindent(listindent);
    printf("%ld objects type %ld size %ld at %lx\n",
            (long)node->nelems,
            (long)node->datatype,
            (long)node->datasize,
            (unsigned long)node->data);
   }
   else
   {
    pindent(listindent);
    printf("%ld objects type %ld size %ld offset %lx\n",
            (long)node->nelems,
            (long)node->datatype,
            (long)node->datasize,
            (unsigned long)((char *)node->data - (long)base));
  }
  if (align_delta((sds_off_t)node->data, node->align) != 0)
  {
    pindent(listindent);
    printf("!!! alignment error\n\n");
  }
}

/******************************************************************
     EXPORT Level 1
struct node_control * sds_new_array(list_index, multiplicity, datatype)
  Book space for a new array of primitive type 'datatype',
  and indicated multiplicity.
******************************************************************/
struct node_control *
sds_new_array(list_index, multiplicity, datatype)
int list_index;
long multiplicity;
long datatype;
{
  struct node_control *node;

  node = sds_new_data(list_index, multiplicity, datatype, 0, -1, NULL);
  if (node != NULL)
  {
    node->sds = 0;
    node->datatype = datatype;
  }
  else
    sds_push_error(SDS_NO_SUCH_OBJ, SDS_WARNING, "Array node not created");
  return node;
}

/******************************************************************
     EXPORT Level 1
int sds_downlist(list_index, node)
  Declare the node to have the indicated list as its downlist.
******************************************************************/
int
sds_downlist(list_index, node)
int list_index;
struct node_control *node;
{
  struct list_control *list;
  if ((list = goodlist(list_index)) == NULL)
    return 0;
  if (node->down != 0)
    sds_push_error(0, SDS_WARNING, "Overwriting existing downlist");
  node->down = list;
  return 1;
}

/******************************************************************
struct node_control *sds_new_struct()
  Ask for the node to create a structure as defined by the sds,object pair.
  Data space is taken from a databank or is malloc()'d
******************************************************************/
struct node_control *
sds_new_struct(list_index, multiplicity, sds, object)
int list_index;
long multiplicity;
sds_handle sds,object;
{
  struct node_control *node = NULL;
  sds_handle code;

  if ((code = sds_obind2code(sds,object)) == 0)
  {
    char string[128];
    sprintf(string,"Object %ld does not exist", (long)sds);
    sds_push_error(SDS_NO_SUCH_OBJ, SDS_WARNING, string);
    return node;
  }
  node = sds_new_data(list_index, multiplicity, code, sds, object, NULL);
  if (node == NULL)
    sds_push_error(SDS_NO_SUCH_OBJ, SDS_WARNING, "Structure node not created");
  return node;
}

/******************************************************************
     INTERNAL
struct node_control * sds_new_data()
  Creates or reserves space, or, if data is not null, registers space
  as being controlled by a node.
******************************************************************/
struct node_control *
sds_new_data(list_index, multiplicity, datatype, sds, object, data)
int list_index;
long multiplicity;
long datatype;
sds_handle sds,object;
void *data;
{
  struct list_control *list;
  struct node_control *node = NULL;
  long dsize;
  char align;

  if ((list = goodlist(list_index)) == NULL)
    return NULL;

  dsize = sds_psize(datatype);
  align = sds_psize(datatype);

  if (data == NULL) /* must create or reserve the data space */
  {
    if (list->do_malloc)
    {
      node = sds_new_node();
      if (!sds_add_node(list, node))
        sds_push_error(SDS_NO_SUCH_LIST, SDS_WARNING, "Failed to add node");
      node->datatype = datatype;
      node->sds = sds;
      node->object = object;
      node->datasize = dsize;
      node->align = align;
      node->nelems = multiplicity;
      node->data = (void *)sds_malloc(node->datasize * multiplicity);
      node->data_was_malloced = 1;
    }
    else
    {
      long shift = align_delta(list->bankused, align);
      if ((list->banksize - list->bankused - shift) < 
                          dsize * multiplicity)
        sds_bank_compress(list_index);
      if ((list->banksize - list->bankused  -shift) >=
                          dsize * multiplicity)
      {
        node = sds_new_node();
        if (!sds_add_node(list, node))
          sds_push_error(SDS_NO_SUCH_LIST, SDS_WARNING, "Failed to add node");
        node->datatype = datatype;
        node->sds = sds;
        node->object = object;
        node->datasize = dsize;
        node->align = align;
        node->nelems = multiplicity;
        list->bankused += shift;
        node->data = (char *)list->databank + list->bankused;
        list->bankused += node->datasize * multiplicity;
      }
      else
      {
        char string[128];
        sprintf(string,"Bank %s exhausted", list->name);
        sds_push_error(SDS_NO_SUCH_OBJ, SDS_WARNING, string);
      }
    }
  }
  else
  {
    node = sds_new_node();
    if (!sds_add_node(list, node))
      sds_push_error(SDS_NO_SUCH_LIST, SDS_WARNING, "Failed to add node");
    node->datatype = datatype;
    node->sds = sds;
    node->object = object;
    node->datasize = dsize;
    node->align = align;
    node->nelems = multiplicity;
    node->data = data;
  }
  return node;
}

/******************************************************************
void sds_remove_data(node)
******************************************************************/
void
sds_remove_data(node)
struct node_control *node;
{
  if (node == NULL || node->marked_for_delete)
  {
    sds_push_error(SDS_NO_SUCH_OBJ, SDS_WARNING, "Removing non-existant node");
    return;
  }
  node->marked_for_delete = 1;
}

/******************************************************************
     INTERNAL
int sds_add_node(list, node)
******************************************************************/
int
sds_add_node(list, node)
struct list_control *list;
struct node_control *node;
{
  if (node == NULL)
  {
    sds_push_error(SDS_NO_SUCH_OBJ, SDS_WARNING, "Adding null node");
    return 1;
  }
  node->up = list;
  if (list->head == NULL)
  {
    list->head = node;
    list->tail = node;
    node->next = NULL;
    node->prev = NULL;
  }
  else
  {
    list->tail->next = node;
    node->prev = list->tail;
    list->tail = node;
  }
  if (list->is_homogeneous)
  {
    node->sds = list->sds;
    node->object = list->object;
    node->datatype = list->datatype;
  }
  list->nnodes++;
  list->current = node;
  return 1;
}

/******************************************************************
     INTERNAL
struct node_control * sds_new_node(void)
******************************************************************/
struct node_control *
sds_new_node(VOIDDEF)
{
  struct node_control *node = 
       (struct node_control *)malloc(sizeof(struct node_control));
  sds_init_node(node);
  node->node_was_malloced = 1;
  return node;
}

/******************************************************************
int    INTERNAL
long sds_delete_node(node)
******************************************************************/
int
sds_delete_node(node)
struct node_control *node;
{
  struct list_control *list;
  if (node == NULL)
  {
    sds_push_error(SDS_NO_SUCH_OBJ, SDS_WARNING, "node already deleted");
    return 0;
  }
  list = node->up;
  if (list->is_deleted)
  {
    sds_push_error(SDS_NO_SUCH_LIST, SDS_ERROR, "List index deleted");
    return 0;
  }
  if (node->prev != NULL)
  {
    if (list->current == node)
      list->current = node->prev;
    if (node->next != NULL) /* prev and next exist */
    {
      node->next->prev = node->prev;
      node->prev->next = node->next;
    }
    else /* prev, but no next */
    {
      list->tail = node->prev; 
      node->prev->next = NULL;
    }
  }
  else /* no prev */
  {
    list->head = node->next;
    if (node->next != NULL) /* next, but no prev */
      node->next->prev = NULL;
    else /* no next or prev */
      list->tail = NULL;
    if (list->current == node)
      list->current = list->head;
  }
  if (node->data_was_malloced)
    free((char *)node->data);
  if (node->node_was_malloced)
    free((char *)node);
  list->nnodes--;
  return 1;
}

/******************************************************************
     INTERNAL
void sds_bank_compress(list_index)
******************************************************************/
void
sds_bank_compress(list_index)
int list_index;
{
  sds_listcon *slc = sds_listc();
  struct node_control *node = slc->lc[list_index - 1].head;
  struct node_control *scan;
  while (node != NULL)
  {
    if (node->marked_for_delete && !node->data_was_malloced)
    {
      void *banknow = node->data;
      long size;
      scan = node;
      while((scan = scan->next) != NULL)
      {
        unsigned char align = 0;
        if (scan->next != NULL)
          align = scan->next->align;
        banknow = (void *)((long)banknow + align_delta((off_t)banknow, align));
        size = scan->datasize * scan->nelems;
        memcpy(banknow, scan->data, (int)size);
        scan->data = banknow;
        banknow = (void *)((long)banknow + size);
      }
      slc->lc[list_index - 1].bankused = 
           (long)banknow - (long)slc->lc[list_index - 1].databank;
      scan = node;
      sds_delete_node(node);
      node = scan->next;
    }
    else
      node = node->next;
  }
}

/******************************************************************
     INTERNAL
void
sds_init_node(node)
******************************************************************/
void
sds_init_node(node)
struct node_control *node;
{
  node->next = NULL;
  node->prev =  NULL;
  node->up = NULL;
  node->down = NULL;
  node->data = NULL;
  node->datasize = 
  node->sds = 
  node->object = 0;
  node->name = NULL;
  node->marked_for_delete =
  node->node_was_malloced = 0;
  node->data_was_malloced = 0;
}

/******************************************************************
long sds_make_as_list(name, id,sds,object)
******************************************************************/
int
sds_make_as_list(name, id,sds,object)
char *name;
int id;
sds_handle sds,object;
{
  int list_index;
  struct list_control *list;
  sds_code mult,code,count,dsize;
  void *data;

  list_index = sds_new_list(name,id);
  if ((list = goodlist(list_index)) == NULL)
    return 0;

  if ((code = sds_obind2code(sds,object)) == 0)
  {
    char string[128];
    sprintf(string,"Object %ld from sds %ld does not exist", (long)object,(long)sds);
    sds_push_error(sds, SDS_WARNING, string);
    return 0;
  }
  mult = sds_array_size(sds,object);
  data = sds_obind2ptr(sds,object);
  dsize = sds_psize(code);
  for (count = 0;count < mult; count++)
  {
    sds_new_data(list_index,1,code,sds,object,data);
    if (sds_get_object_type(sds,object) == SDS_VARIABLE_LENGTH)
      data = sds_estart(sds)[object]; 
    else
      data = (void *)((long)data + dsize);
  }
  list->is_homogeneous = 1;
  list->sds = sds;
  list->object = object;
  return list_index;
}

/******************************************************************
int sds_allnodes(void)
******************************************************************/
int 
sds_allnodes(VOIDDEF)
{
  int i;
  sds_listcon *slc = sds_listc();
  int total_nodes = 0;
  for (i=0;i<slc->lastlist;i++)
    total_nodes += slc->lc[i].nnodes;
  return total_nodes;
}

/******************************************************************
int sds_concat_list()
******************************************************************/
int
sds_concat_list(sds,nodes,lists, nindex, lindex, l)
sds_handle sds;
struct node_control *nodes;
struct list_control *lists;
long *nindex, *lindex, l;
{
  sds_listcon *slc = sds_listc();
  struct list_control *list = &lists[*lindex];
  if (!slc->lc[l].done_output)
  {
    if (!slc->lc[l].is_deleted)
    {
      memcpy((char *)list,(char *)&slc->lc[l],sizeof(struct list_control));
      sds_concat_nodes(sds,nodes,lists, nindex, lindex);
      if (list->name != NULL)
        list->name = (char *)sds_add_to_heap(sds,slc->lc[l].name, (char)0);
      (*lindex)++;
      slc->lc[l].done_output = 1;
      return *lindex - 1;
    }
  }
  else
  {
    long i = 0;
    while (&lists[i] != &slc->lc[l])
      i++;
    return i + 1;
  }
  return 0;
}

/******************************************************************
void sds_save_lists(filename)
******************************************************************/
void
sds_save_lists(filename)
char *filename;
{
  long i;
  sds_listcon *slc = sds_listc();
  sds_handle sds, sdsinfo;
  sds_code typecode;
  long nindex = 0, lindex = 0;
  sds_record_handle *rh;
  struct stat databuf;
  struct sds_control_p *scp;


  struct node_control *nodes;
  struct list_control *lists; 

  int total_nodes = sds_allnodes();

  sds = sds_new("tree");

  lists = (struct list_control *)
            sds_malloc(slc->lastlist * sizeof(struct list_control));
  nodes = (struct node_control *)
            sds_malloc(total_nodes * sizeof(struct node_control));

  clean_lists();
  for (i=0;i<slc->lastlist;i++)
    sds_concat_list(sds,nodes,lists,&nindex, &lindex, i);
  clean_lists();

  typecode = sds_define_structure(sds,nodec_tl,nodec_names);
  sds_declare_structure(sds,nodes,"Nodes", total_nodes, typecode);
  typecode = sds_define_structure(sds,listc_tl,listc_names);
  sds_declare_structure(sds,lists,"Lists", lindex, typecode);

  for (sdsinfo=1;sdsinfo<=sds_max();sdsinfo++)
  {
    char name[128];
    int mdate;
    int size;
    short index;
    scp = sds_control(sdsinfo);
    if (scp && scp->load_name && sdsinfo != sds)
    {
      stat(scp->load_name , &databuf);
      mdate = databuf.st_mtime;
      size = databuf.st_size;
      index = sdsinfo;
      strcpy(name,"SDS:");
      strcat(name ,sds_obind2name(sdsinfo,0));
      rh = sds_begin_record(name);
      sds_record_entry(rh, SDS_SHORT, 1, &index, "index");
      sds_record_entry(rh, SDS_STRING, strlen(scp->load_name) + 1,
                                                     scp->load_name, "name");
      sds_record_entry(rh, SDS_INT, 1, &scp->source, "type");
      sds_record_entry(rh, SDS_UNIX_TIME, 1, &mdate, "mod_date");
      sds_record_entry(rh, SDS_LONG, 1, &size, "size");
      sds_end_and_declare(rh,sds);
    }
  }

  sds_ass(sds,filename, SDS_FILE);
  sds_destroy(sds);
  free(lists);
  free(nodes);
}
  
/******************************************************************
void sds_concat_nodes(sds, nodes,list, nindex, lindex)
******************************************************************/
void
sds_concat_nodes(sds,nodes,lists, nindex, lindex)
sds_handle sds;
struct node_control *nodes;
struct list_control *lists;
long *nindex, *lindex;
{
  struct list_control *list = &lists[*lindex];
  struct node_control *n = list->head;
  sds_listcon *slc = sds_listc();

  list->head = (void *)*nindex; /*looks odd but for internalised pointer */
  while (n != NULL)
  {
    memcpy((char *)&nodes[*nindex], (void *)n, sizeof(struct node_control));
    if (n->name != NULL)
      nodes[*nindex].name = (void *)sds_add_to_heap(sds,n->name, (char)0);
    if (n->prev != NULL)
      nodes[*nindex -1].next = (void *)*nindex;
    if (n->next != NULL)
      nodes[*nindex].next = (void *)(*nindex + 1);
    else
      list->tail = (void *)*nindex;
    if (n->down!= NULL)
    {
      long li;
      for (li = 0;li < slc->lastlist; li++)
        if (&lists[li] == n->down)
        {
          nodes[*nindex].down =
               (void *)sds_concat_list(sds,nodes,lists,nindex,lindex,li);
          break;
        }
    }
    (*nindex)++;
    n = n->next;
  }
}

sds_code 
sds_get_size(sds, elemtype)
sds_code elemtype;
sds_handle sds;
{
  if (elemtype < (long)NTYPES) /* It's a primitive */
     return sds_psize(elemtype);
  else
  {
    struct type_list *tl = sds_tlist(sds);
    tl = &tl[(elemtype & ~SDS_INDLIST) + 1];
    return (tl->nelems);
  }
}
struct node_control *
sds_get_current(list)
struct list_control *list;
{
  return list->current;
}
/******************************************************************
struct node_control * sds_add()
******************************************************************/
struct node_control *
sds_add(list, name, data, id, size, type)
struct list_control *list;
char *name;
void *data;
int id;
int size;
int type;
{
  struct node_control *node = NULL;
  char align;

  align = sds_psize(type);

  node = sds_new_node();
  if (!sds_add_node(list, node))
    sds_push_error(SDS_NO_SUCH_LIST, SDS_WARNING, "Failed to add node");
  node->datatype = type;
  node->sds = 0;
  node->object = id;
  node->datasize = size;
  node->align = align;
  node->nelems = 1;
  node->data = data;
  node->name = name;
  return node;
}

struct node_control *
sds_next(list,prev)
struct list_control *list;
struct node_control *prev;
{
  struct node_control *n;
  if (list->is_deleted == 1)
    return 0;

  if (!prev)
		sds_gobefore(list,0);

  if (list->current == 0)
	{
		list->current = list->head;
		return 0;
	}
  if (prev && list->current->prev != prev)
    sds_gobefore(list,prev);

	n = list->current;
  list->current = list->current->next;
  return n;
}

int
sds_gobefore(list,node)
struct list_control *list;
struct node_control *node;
{
	if (node == 0)
		list->current = node = list->head;
  if (list->current == node)
    return 1;
  list->current = list->head;
  if (node != 0)
  {
    while(list->current != node)
		{
      list->current = list->current->next;
			if (!list->current)
				return 0;
		}
  }
	return 1;
}

void
sds_no_lists() { listsleft = 0; }
