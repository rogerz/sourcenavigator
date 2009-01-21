/* $Header$ */


#include "Sds/sdsgen.h"
#include "Sds/sds_externs.h"
#include "Sds/sds_tree.h"
#include "Sds/sds_object.h"
#include <string.h>
#include <time.h>
#include <values.h>
#if defined(__MSDOS__)
#include <alloc.h>
#else
#include <malloc.h>
#endif

#define SDS_INITIAL_TNODE 0x01

/********** forward declarations **********/
#if defined (__STDC__)
sds_tnode       * look_on_row(sds_tnode *, char *,int);
char            * nextname(char **);
void              printdesc(sds_tnode *,int,int);
void              printinst(sds_tnode *,int,int);
sds_tnode       * sds_find(int,sds_tnode *, char *,int);
void              printres(sds_tnode *, int);
int               countstruct(sds_tnode *);
struct type_list* maketlist(int);
sds_code          makestruct(sds_tnode *,int *,
                             struct type_list *, struct direc *, int);
int               sds_gnt(void);
sds_tnode *       sds_dup_down(int, sds_tnode *, sds_tnode *);

#else
sds_tnode       * look_on_row();
char            * nextname();
void              printdesc();
void              printinst();
sds_tnode       * sds_find();
void              printres();
struct type_list* maketlist();
sds_code          makestruct();
int               sds_gnt();
sds_tnode *       sds_dup_down();

#endif

int 
sds_tree_handle(sds)
sds_handle sds;
{
  struct sds_control_p *scp = sds_control(sds);
  return scp->tree;
}

int
sds_new_tree(data)
void *data;
{
  sds_treecon *stc = sds_treec();
  int ntree = sds_gnt();
  stc->top[ntree] = sds_new_tnode(data);
  stc->top[ntree]->top = stc->top[ntree];
  stc->top[ntree]->head = stc->top[ntree];
  stc->top[ntree]->ntree = ntree;
  stc->tpos[ntree].ntnodes = 1;
  return ntree + 1;
}

int 
sds_gnt(VOIDDEF)
{
  sds_treecon *stc = sds_treec();
  int i = 0;
  while (i < stc->next_tree && stc->top[i] != 0)
    i++;
  if (i == stc->next_tree)
    stc->next_tree++;
  if (stc->next_tree > sds_max())
  {
    sds_push_error(SDS_NO_SPC, SDS_WARNING," No tree space left");
    return -1;
  }
  return i;
}

sds_tnode *
sds_get_top(itree)
int itree;
{ 
  sds_treecon *stc = sds_treec();
  return itree?stc->top[itree - 1]:0;
}

sds_tnode *
sds_down_tnode(tnode, data)
sds_tnode *tnode;
void *data;
{
  sds_tnode *ndown;
  sds_treecon *stc = sds_treec();
  if (tnode == 0)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_WARNING,"Down node is null");
    return 0;
  }
  ndown = tnode->down;
  tnode->down = sds_new_tnode(data);
  tnode->down->top = tnode;
  tnode->down->head = tnode->down;
  tnode->down->ntree = tnode->ntree;
  tnode->down->down = ndown;
  stc->tpos[tnode->ntree].ntnodes++;
  return  tnode->down;
}

sds_tnode *
sds_next_tnode(tnode, data)
sds_tnode *tnode;
void *data;
{
  sds_treecon *stc = sds_treec();
  sds_tnode *nnext;
  if (tnode == 0)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_WARNING,"Next node is null");
    return 0;
  }
  nnext = tnode->next;
  tnode->next = sds_new_tnode(data);
  tnode->next->top = tnode->top;
  tnode->next->head = tnode->head;
  tnode->next->ntree = tnode->ntree;
  tnode->next->next = nnext;
  stc->tpos[tnode->ntree].ntnodes++;
  return  tnode->next;
}

sds_tnode *
sds_new_tnode(data)
void *data;
{
  sds_tnode *n = (sds_tnode *)malloc(sizeof(sds_tnode));
  n->data = data;
  n->next = 0;
  n->down = 0;
  n->top = 0;
  return n;
}

void
sds_start_tree(itree, tnode)
int itree;
sds_tnode *tnode;
{
  if (itree)
  {
    sds_treecon *stc = sds_treec();
    stc->trav[itree-1] = tnode;
    stc->tpos[itree-1].width = 
    stc->tpos[itree-1].depth = (short)0;
    if (!tnode)
    {
      stc->tpos[itree-1].flag = 0;
      stc->tpos[itree-1].top = stc->top[itree-1];
    }
    else
    {
      stc->tpos[itree-1].flag |= SDS_INITIAL_TNODE;
      stc->tpos[itree-1].top = tnode;
    }
  }
  else
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Illegal tree handle");
}

tree_pos *
sds_get_pos(itree)
int itree;
{ 
  sds_treecon *stc = sds_treec();
  return itree?&stc->tpos[itree-1]:0; 
}

sds_tnode *
sds_dup_down(itree, ttnode, dtnode)
int itree;
sds_tnode *ttnode, *dtnode;
{
  sds_desc *td,*d = (sds_desc *)malloc(sizeof(sds_desc));
  sds_tnode *t,*n, *end;
  if (itree < 0 ) /* sds_trav_tree is not re-entrant, so must signal
                     the first entry       
                   */
  {
    itree = -itree;
  }
  else
  {
    sds_start_tree(itree, ttnode);
    t = sds_trav_tree(itree);
  }
  end = ttnode->next;
  t = sds_trav_tree(itree);
  memcpy((char *)d, (char *)t->data, sizeof(sds_desc));
  n = sds_down_tnode(dtnode,d);
  while ((t = sds_trav_tree(itree)) != end)
  {
    td = (sds_desc *)t->data;
    if (td->type & SDS_INDLIST)
    {
      t = sds_dup_down(-itree,t,n);
      td = (sds_desc *)t->data;
    }
    if (t != end)
    {
      d = (sds_desc *)malloc(sizeof(sds_desc));
      memcpy((char *)d, (char *)td, sizeof(sds_desc));
      n = sds_next_tnode(n, d);
    } 
  }
  return t;
}

sds_tnode *
sds_trav_tree(itree)
int itree;
{
  sds_tnode *n,*next;
  sds_treecon *stc = sds_treec();

  if (!itree) return 0;

  n = stc->trav[itree-1];
  next = 0;
  if (stc->tpos[itree-1].flag & SDS_INITIAL_TNODE)
  {
    stc->tpos[itree-1].flag &= ~SDS_INITIAL_TNODE;
    next = n;
  }
  else if (!n)
  {
    next = stc->top[itree-1];
    stc->tpos[itree-1].width = stc->tpos[itree-1].depth = 0;
  }
  else if (n->down)
  {
    next = n->down;
    stc->tpos[itree-1].width = 0;
    stc->tpos[itree-1].depth++;
  }
  else if (n->next)
  {
    next = n->next;
    stc->tpos[itree-1].width++;
  }
  else if (n->head == stc->tpos[itree-1].top)
    next = 0;
  else
  {
    while ((next = n->top->next) == 0)
    {
      n = n->top;
      stc->tpos[itree-1].depth--;
      if (!stc->tpos[itree-1].depth)
      {
        next = 0;
        stc->tpos[itree-1].depth++;
        break;
      }
    }
    stc->tpos[itree-1].depth--;
  }
  stc->trav[itree-1] = next;
  return next;
}

void 
sds_delete_tree(itree,stnode, truefalse)
int itree,truefalse;
sds_tnode *stnode;
{
  sds_tnode *n,**N;
  sds_treecon *stc = sds_treec();
  int i = 0,count = 0;;

  if (!itree) return;

  N = (sds_tnode **)
             malloc(stc->tpos[itree-1].ntnodes * sizeof(sds_tnode *));
  sds_start_tree(itree, stnode);
  while ((n = sds_trav_tree(itree)) != 0)
  {
    N[i++] = n;  
    count++;
  }
  for (i=0;i<count;i++)
  {
    if (truefalse)
    {
      if (itree == stc->inst_tree)
      {
        sds_inst *in = (sds_inst *)(N[i]->data);
        if (!(--in->refcount))
          free(N[i]->data);
      }
      else
      {
        sds_desc *d = (sds_desc *)(N[i]->data);
        if (!(--d->refcount))
          free(N[i]->data);
      }
    }
    free(N[i]);
  }
  if (count != stc->tpos[itree-1].ntnodes)
    sds_push_error(SDS_NO_SPC,SDS_ERROR,
                           "Tree nodes not accounted for in delete");
  stc->top[itree-1] = 0;
  free(N);
}

sds_tnode *
sds_res(itree)
int itree;
{
  sds_tnode *p,*n,*next;
  sds_desc *d;
  sds_treecon *stc = sds_treec();

  if (!itree) return 0;
  n = stc->trav[itree-1];
  next = 0;

  if (n) /* Current node loaded */
    d = (sds_desc *)n->data;
  if (!n) /* Starting the tree; initialise the structure counters */
  {
    while ((p = sds_trav_tree(itree)) != 0)
    {
     d = (sds_desc *)p->data;
     if (d->type & SDS_INDLIST)
       d->mark = d->nelems;
    }
    next = stc->top[itree-1];
  }
  else if (n->down) /* Go down if possible */
    next = n->down;
  else if (n->next) /* or get the next in the list */
    next = n->next;
  else if (n->head == stc->tpos[itree-1].top) /* exit if we're at the top */
    next = 0;
  else if (((sds_desc *)(n->top->data))->mark) /* or get the next structure */
    next = n->top;
  else /* We've done all the structures, keep going up */
  {
    ((sds_desc *)(n->top->data))->mark = ((sds_desc *)(n->top->data))->nelems;
    while ((next = n->top->next) == 0)
    {
      n = n->top;
      if (n == stc->top[itree-1])
      {
        next = 0;
        break;
      }
    }
  }
  stc->trav[itree-1] = next;
  if (next)
  {
    d = (sds_desc *)next->data;
    if (d->type & SDS_INDLIST)
      d->mark--;
  }
  return next;
}

sds_tnode * 
sds_check_sizes(itree)
int itree;
{
  sds_tnode *n;
  sds_desc *d;
  int fp = 1;
  long start = 0L;

  sds_start_tree(itree, 0);
  while ((n = sds_trav_tree(itree)) != 0)
  {
    d = (sds_desc *)n->data;
    if (fp)
    {
      fp = 0;
      if (d->type != SDS_SDS)
        start = d->offset;
    }
    printf("%s [%d] sizes %x %x, align %d %d, addrs %x %x\n",d->name,
             d->nelems, d->size, d->ssize,
             (int)d->align & 0xff,(int)d->salign & 0xff,
             (int)d->offset - start, (int)d->soffset);
  }
  return 0;
}

sds_inst * 
sds_new_inst(nelems, name, type)
sds_code nelems,type;
char *name;
{
  sds_inst *i = (sds_inst *)malloc(sizeof(sds_inst));
	i->name = name;
	i->number = nelems;
	i->pointer = 0;
	i->def = 0;
	i->was_allocated = 0;
	i->refcount = 0;
	return i;
}

sds_desc *
sds_new_desc(nelems, name, type)
sds_code nelems,type;
char *name;
{
  sds_desc *d = (sds_desc *)malloc(sizeof(sds_desc));
  d->nelems = nelems;
  d->name = name;
  d->type = type;
  d->size = 0L;
  d->ssize = 0L;
  d->offset = 0L;
  d->soffset = 0L;
  d->align = (char)0;
  d->salign = (char)0;
  d->mark = 0L;
  d->refcount = (char)1;
  return d;
}

void 
sds_print_tree(itree,values)
int itree,values;
{
  sds_treecon *stc = sds_treec();
  tree_pos * pos = sds_get_pos(itree);
  sds_tnode *n;

  if (itree == stc->inst_tree)
    while ((n = sds_trav_tree(itree)) != 0)
      printinst(n, (int)pos->depth, values);
	else
    while ((n = sds_trav_tree(itree)) != 0)
      printdesc(n, (int)pos->depth, values);
}

void
sds_makeaddr(itree,nstart,sstart)
int itree;
void * nstart,* sstart;
{
  sds_tnode *n;
  sds_desc *d;
  long off = (long)nstart,soff = (long)sstart;

  while ((n = sds_trav_tree(itree)) != 0)
  {
    d = (sds_desc *)n->data;
    off += align_delta(off, d->align);
    soff += align_delta(soff, d->salign);
    d->offset = off;
    d->soffset = soff;
    if (!(d->type & SDS_INDLIST) && (d->type != SDS_BITFIELD))
    {
      off += d->size * d->nelems;
      soff += d->ssize * d->nelems;
    }
  }
}

void
printres(n,arc)
sds_tnode *n;
int arc;
{
  sds_desc *d = (sds_desc *)n->data;
  if (d->type < NPRIMITIVES && d->type != SDS_SDS)
    printf("%d %s, %d bytes on %d at %x: on %d\n",d->nelems,
                     type_name[d->type],
                     d->size,
                     (int)d->align & 0xff,
                     d->offset,
                     (int)sds_arc_palign(arc, d->type) & 0xff);
}

void
printinst(n, depth, values)
sds_tnode *n;
int depth;
int values;
{
  sds_inst *i;

  pindent(depth);
  i = (sds_inst *)n->data;
	printf("Object %s, multiplicity %d at %x,",i->name,i->number,i->pointer);
	if (i->was_allocated)
		printf("allocated,");
  printf("refcount %d\n",i->refcount);
	if (i->def)
		printdesc(i->def,depth,values);
  else
	{
    pindent(depth);
		printf("Primitive %s, %d\n",type_name[i->code],i->number);
	}
}

void
printdesc(n, depth, values)
sds_tnode *n;
int depth;
int values;
{
  sds_desc *d;

  pindent(depth);
  d = (sds_desc *)n->data;
  printf("%s", d->name);
  if (d->nelems > 1)
    printf("[%d]",d->nelems);
  if (d->type < NPRIMITIVES)
    printf(" (%s)",type_name[d->type]);
  else
    printf(" (Structure)");
		/* Must check ntree here for defn only */
  if (values && d->type < NPRIMITIVES)
  {
    switch (d->type)
    {
      char *tim;
      case SDS_SHORT:
        printf(" value: %d", *(short *)d->offset);
      break;
      case SDS_LONG:
        printf(" value: %d", *(int *)d->offset);
      break;
      case SDS_CHAR:
        printf(" value: %d", (int)*(char *)d->offset & 0xff);
      break;
      case SDS_POINTER:
        printf(" value: %x", *(int *)d->offset);
      break;
      case SDS_UNIX_TIME:
        tim = ctime((time_t *)d->offset);
        tim[strlen(tim) - 1] = (char)0;
        printf(" value: %s", tim);
      break;
      case SDS_STRING:
        printf(" value: %s", (char *)d->offset);
      break;
      case SDS_FSTRING:
        printf(" value: %c", *(char *)d->offset);
      break;
      case SDS_FLOAT:
        printf(" value: %e", *(float *)d->offset);
      break;
      case SDS_DOUBLE:
        printf(" value: %e", *(double *)d->offset);
      break;
      default:
      break;
    }
  }
  if (depth == SDS_OBJECT_LEVEL)
    printf(" (Object)");
  printf("\n");
}

int 
sds_tree(sds, object, itree)
sds_handle sds,object;
int itree;
{
  struct direc *dptr;
  sds_tnode *n;

  if (!itree)
  {
    itree = sds_new_tree(sds_new_desc(1,sds_obind2name(sds,0),SDS_SDS));
    n = sds_get_top(itree);
  }
  else
  {
    n = sds_get_top(itree);
  }
  if (!itree)
  {
    sds_push_error(SDS_NO_SPC,SDS_WARNING,"Could not create new tree");
    return 0;
  }
  if ((dptr = sds_direc(sds)) == 0)
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_WARNING,"Could not find dataset");
    return 0;
  }
  if (object < 1L || object >= dptr[0].nelems)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_WARNING,"Object not in dataset");
    return 0;
  }

  n = maketree(sds_tlist(sds),
           dptr[object].elemcod,
           itree,
           n,
           sds_heap(sds),
           dptr[object].nelems,
           sds_obind2name(sds,object), 
           SDS_SUN3ARC);
  sds_start_tree(itree, n);
  sds_makeaddr(itree, (void *)sds_obind2ptr(sds,object), (void *)0);

  return itree;
}

sds_tnode *
maketree(tlist,code,itree,tnode,nheap,number,name, arc)
struct type_list *tlist;
sds_code          code;
int               itree;
sds_tnode         *tnode;
char             *nheap;
sds_code          number;
char             *name;
int               arc;
{
  sds_code index;
  sds_desc *d,*bitd,*c = (sds_desc *)tnode->data;
  char *tname = 0;
  sds_tnode *dtnode = 0, *ctnode = tnode->down;
  char nativealign = 0;
  char sourcealign = 0;
  long nativesize = 0L;
  long sourcesize = 0L;
  int indindex = -1;
  int bitsused = 0;

  index = code & ~SDS_CODE_MASK;
  d = sds_new_desc(number,name,code);

  
  /* Find the 'current' tnode */
  if (!ctnode)
    ctnode = sds_down_tnode(tnode,d);
  else
  {
    while(ctnode->next)
      ctnode = ctnode->next;
    ctnode = sds_next_tnode(ctnode,d);
  }

  /* This is a compound data type..... */
  if ((code & SDS_INDLIST))
  {
    if ((code = tlist[index].elemcod) & SDS_LENLIST)
      tname = nheap + (tlist[index++].nelems & 0xffff);
    if ((code = tlist[index].elemcod) & SDS_SIZE_ALIGN)
      indindex = index++;
  }
  else /* .....or simple array */
  {
    d->type = code;
    d->size = sds_psize(code);
    d->ssize = sds_psize(code);
    d->nelems = number;
    d->align = (char)sds_palign(code);
    d->salign = sds_arc_palign(arc, d->type);
    return ctnode;
  }

  while (!((code = tlist[index].elemcod) & SDS_RETLIST))
  {
    number = tlist[index].nelems;
    if ((code & SDS_INDLIST))
    {
      maketree(tlist, code, itree, ctnode, nheap, number, tname, arc);
      dtnode = ctnode->down;
      while(dtnode->next)
        dtnode = dtnode->next;
      d = (sds_desc *)dtnode->data;
    }
    else if ((code & SDS_OBJECT_DEF))
    {
      sds_tnode * ttnode = sds_findname(itree,tname);
      if (ttnode == (sds_tnode *)0)
      {
        sds_push_error(SDS_NO_SUCH_OBJ,SDS_FATAL,
             "Unresolved reference defining object");
        return 0;
      }
      d = sds_new_desc(number,"",code);
      memcpy((char *)d, (char *)ttnode->data, sizeof(sds_desc));
      d->nelems = number;
      if (!dtnode)
      {
        c = (sds_desc *)ctnode->data;
        dtnode = sds_down_tnode(ctnode,d);
      }
      else
      {
        while (dtnode->next)
          dtnode = dtnode->next;
        c = (sds_desc *)dtnode->data;
        dtnode = sds_next_tnode(dtnode,d);
      }
      sds_dup_down(itree, ttnode, dtnode);
    }
    else
    {
      d = sds_new_desc(number,tname,code);
      if(d->type == SDS_BITFIELD)
      {
        bitd = d;
        if (!bitsused)
        {
          d->align = (char)sds_palign(SDS_INT);
          d->size = sds_psize(SDS_INT);
          d->salign = sds_arc_palign(arc, SDS_INT);
          d->ssize = d->size;
        }
        else
        {
          d->align = (char)0;
          d->size = 0L;
          d->salign =(char)0;
          d->ssize = 0L;
        }
        bitsused += d->nelems;
        if (bitsused == BITS(int))
        {
          d->type = SDS_END_BITFIELDS;
          bitsused = 0;
        }
      }
      else
      {
        if (bitsused)
        {
          bitsused = 0;
          bitd->type = SDS_END_BITFIELDS;
        }
        d->align = (char)sds_palign(tlist[index].elemcod);
        d->size = sds_psize(tlist[index].elemcod);
        d->salign = sds_arc_palign(arc, d->type);
        d->ssize = d->size;
      }

      if (!dtnode)
      {
        c = (sds_desc *)ctnode->data;
        dtnode = sds_down_tnode(ctnode,d);
      }
      else
      {
        while (dtnode->next)
          dtnode = dtnode->next;
        c = (sds_desc *)dtnode->data;
        dtnode = sds_next_tnode(dtnode,d);
      }
    }
    sourcealign =  (d->salign > sourcealign)?d->salign:sourcealign;
    nativealign =  (d->align > nativealign)?d->align:nativealign;
    sourcesize += align_delta(sourcesize,d->salign);
    nativesize += align_delta(nativesize,d->align);
    if (d->type != SDS_BITFIELD)
    {
      nativesize += d->size * d->nelems;
      sourcesize += d->ssize * d->nelems;
    }
    else
    {
      nativesize += d->size;
      sourcesize += d->ssize;
    }
    tname = strchr(tname,(char)0) + 1;
    index++;
  }
  d = (sds_desc *)dtnode->top->data;

  d->salign = sourcealign;
  sourcesize += align_delta(sourcesize,sourcealign);
  d->ssize = sourcesize;

  d->align = nativealign;
  nativesize += align_delta(nativesize,nativealign);
  d->size = nativesize;

  if (indindex != -1)
  {
    tlist[indindex].nelems = d->size;
    tlist[indindex].elemcod = SDS_SIZE_ALIGN | ((long)d->align & 0xff);
  }
  return ctnode;
}

sds_tnode *
sds_findname(itree,name)
int itree;
char *name;
{ return sds_find(itree,0,name,0); }

sds_tnode *
sds_likename(itree,n,name)
int itree;
sds_tnode *n;
char *name;
{ return sds_find(itree,n,name,1); }

sds_tnode *
sds_find(itree,n,name, like)
int itree;
sds_tnode *n;
char *name;
int like;
{
  char *sdsname = 0;
  char *oname = 0;
  sds_desc *d;
  char *endpoint,*startpoint = name;
  int failure = 0;
  int startfromtop = 0;

  if (!n) /* start from top */
  {
    n = sds_get_top(itree);
    startfromtop = 1;
  }
  else
    n = n->next;
  if (!n) /* still 0 - invalid tree or end of list */
    return 0;

  if (startfromtop)
  {
    endpoint = strchr(name,'|');
    if (endpoint)
    {
      int length = endpoint - startpoint;
      failure = 1;
      sdsname = malloc(length);
      strncpy(sdsname,startpoint,length);
      sdsname[length] = (char)0;
      startpoint = endpoint + 1;
      while (n != 0)
      {
        d = (sds_desc *)n->data;
        if ((like && strstr(d->name, sdsname)) ||
            (!like && !strcmp(sdsname,d->name)))
        {
          failure = 0;
          n = n->down;
          break;
        }
      }
      free(sdsname);
      if (failure)
        return 0;
    }
    else
      n = n->down;
  }
  oname = nextname(&startpoint);
  while (n)
  {
    if ((n = look_on_row(n,oname,like)) != 0)
    {
      if ((oname = nextname(&startpoint)) != 0)
        n = n->down;
      else 
        return n;
    }
    else
      return 0;
  }
  return 0;
}

char *
nextname(start)
char **start;
{
  static char * buffer = 0;
  char *end;
  int length;

  if (*start == (char)0)
  {
    if (buffer)
      free(buffer);
    buffer = 0;
  }
  else
  {
    if ((end = strchr(*start,'.')) != 0)
      length = end - *start;
    else
      length = strlen(*start) + 1;
    if (buffer)
      free(buffer);
    buffer = malloc(length+1);
    strncpy(buffer,*start,length);
    buffer[length] = (char)0;
    *start = end?end + 1:0;
  }
  return buffer;
}

sds_tnode *
look_on_row(n, name, like)
sds_tnode *n;
char *name;
int like;
{
  sds_desc *d;
  do
  {
    d = (sds_desc *)n->data;
    if ((like && strstr(d->name, name)) ||
        (!like && !strcmp(name,d->name)))
      return n;
    n = n->next;
  } while (n != 0);
  return 0;
}

struct type_list * 
maketlist(itree)
int itree;
{
  sds_tnode *n;
  sds_desc *d;
  sds_code code;
  int i = 0;
  tree_pos * pos = sds_get_pos(itree);
  int nobs = 0, nlist = 0, index = 0;
  struct type_list *tlist;
  struct direc *dptr;

  sds_start_tree(itree, 0);
  /* count dptr and tlist size */
  while ((n = sds_trav_tree(itree)) != 0)
  {
    d = (sds_desc *)n->data;
    if (d->type & SDS_INDLIST)
    {
      nlist += 3; /* alignment, size and return space */
      if (pos->depth == SDS_OBJECT_LEVEL)
      {
        nobs++;
        nlist--;
      }
    }
    if (!((pos->depth == SDS_OBJECT_LEVEL) && !(d->type & SDS_INDLIST)))
      nlist++;
  }
  printf("%d compound objects, tlist %x\n",nobs,nlist);
  tlist = (struct type_list *)malloc(nlist * sizeof(struct type_list));
  dptr = (struct direc *)malloc((nobs + 1) * sizeof(struct direc));
  tlist[nlist-1].elemcod = SDS_ENDLIST;
  tlist[nlist-1].nelems = 0L;

  makestruct(sds_get_top(itree)->down, &index, tlist, dptr, SDS_OBJECT_LEVEL);

  while ((code = tlist[i].elemcod) != SDS_ENDLIST)
  {
    printf("%x: %x code %x\n",i,tlist[i].nelems, code);
    i++;
  }
  printf("%x: %x code %x\n",i,tlist[i].nelems, code);
  for (i= 1; i<nobs;i++)
    printf("Object %d code %x size %d\n",i,dptr[i].elemcod, dptr[i].elemsz);
  return tlist;
}

sds_code
makestruct(n,index,tlist, dptr, depth)
sds_tnode *n;
int *index,depth;
struct type_list *tlist;
struct direc *dptr;
{
  sds_desc *d;
  int nindex = *index;
  int start = *index;
  int iso = (depth == SDS_OBJECT_LEVEL)?1:0;
  if (!iso)
    nindex += (countstruct(n) + 2);

  do
  {
    d = (sds_desc *)n->data;
    if (!iso)
    {
      tlist[*index].elemcod = d->type;
      tlist[*index].nelems = d->nelems;
    }
    if (d->type & SDS_INDLIST || d->type == SDS_SDS)
    {
      tlist[nindex].elemcod = SDS_LENLIST;
      tlist[nindex].nelems = 0L;
      nindex++;
      tlist[nindex].elemcod = SDS_SIZE_ALIGN | (long)(d->align & 0xff);
      tlist[nindex].nelems = d->size;
      nindex++;
      if (iso)
      {
        printf("Object %d\n",iso);
        dptr[iso].elemcod =
              makestruct(n->down,&nindex,tlist,dptr, depth + 1);
        dptr[iso].elemsz = d->size;
        dptr[iso].align_type = d->align;
        dptr[iso].nelems = d->nelems;
        iso++;
      }
      else
        tlist[*index].elemcod =
              makestruct(n->down,&nindex,tlist,dptr,depth + 1);
    }
    if (!iso)
      (*index)++;
  } while ((n = n->next) != 0);

  if (!iso)
  {
    tlist[*index].elemcod = SDS_RETLIST;
    tlist[*index].nelems = 0L;
    (*index)++;
  }
  return (start - 2) | SDS_INDLIST;
}

int
countstruct(n)
sds_tnode *n;
{
  int i = 0;
  while((n = n->next) != 0)
    i++;
  return i;
}

