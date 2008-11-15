
#include "Sds/sdsgen.h"
#include "Sds/sds_externs.h"
#include "Sds/sds_object.h"
#include <malloc.h>


#if defined(__STDC__)
sds_instance sds_makeobject(sds_code,sds_defined, char *, int, void *);
#else
sds_instance sds_makeobject();
#endif

/*********************************************************************/
int
tlen(tlist)
struct  type_list  *tlist;
/*********************************************************************/
{
  int  len = 0;
  if (tlist == TNULL) return 0;
  while ((tlist++)->elemcod != SDS_ENDLIST) 
    len++;
  return ++len;
}

void
sds_delete_odef(odef)
sds_odef odef;
{
  if (odef->tlist != (struct type_list *)0)
    free((char *)odef->tlist);
  free((char *)odef);
}

/*********************************************************************/
sds_odef
sds_new_odef()
/*********************************************************************/
{
  sds_odef o = (sds_odef)sds_malloc(sizeof(struct sds_odef));
  o->tlist = (struct type_list *)0;
  o->tllength = 0;
  o->gennames = (char *)0;
  return o;
}

sds_instance
sds_instantiate(def, name, number, pointer)
sds_defined def;
char *name;
int number;
void *pointer;
{ return sds_makeobject(0,def, name, number, pointer); }

sds_instance
sds_array(code, name, number, pointer)
sds_code code;
char *name;
int number;
void *pointer;
{ return sds_makeobject(code,0, name, number, pointer); }

sds_instance
sds_makeobject(code,def, name, number, pointer)
sds_code code;
sds_defined def;
char *name;
int number;
void *pointer;
{
  sds_treecon *stc = sds_treec();
  int itree = stc->inst_tree;
  sds_tnode *ctnode = sds_get_top(itree)->down;
  struct sds_inst *i = malloc(sizeof(struct sds_inst));
  i->name = name;
  i->def = def;
  i->code = code;
  i->number = number;
  i->pointer = pointer;
  i->refcount = (char)1;
  if (i->pointer == (void *)0)
  {
    if (i->def)
    {
      sds_desc *d = (sds_desc *)(def->data);
      i->pointer = sds_malloc(i->number * d->size);
    }
    else
    {
      i->pointer = sds_malloc(i->number * sds_psize(code));
    }
    i->was_allocated = 1;
  }
  else
  {
    i->was_allocated = 0;
  }
  if (!ctnode)
    ctnode = sds_down_tnode(sds_get_top(itree),i);
  else
  {
    while(ctnode->next)
      ctnode = ctnode->next;
   ctnode = sds_next_tnode(ctnode,i);
  }
  return (sds_instance)ctnode;
}

/*********************************************************************/
sds_defined 
sds_def_object(name,tlist,namelist)
struct type_list *tlist;
char  *name,*namelist;
/*********************************************************************/
{
  sds_tnode *n;
  sds_treecon *stc = sds_treec();
  char  *tmp;
  sds_odef o = sds_new_odef();
  int otree = stc->odef_tree;
  int ncount = 0;

  o->tllength = tlen(tlist) + 2;
  o->tlist = (struct type_list *)
       sds_malloc(o->tllength * sizeof(struct type_list));
  o->tlist_malloced = 1;
/*  (A size/namelist pair will be added:, and the size/alignment type
  pair:hence the '+2' */

  o->tlist[0].elemcod = SDS_LENLIST;
  o->tlist[0].nelems = 0;
  o->tlist[1].elemcod = SDS_SIZE_ALIGN;
  o->tlist[1].nelems = 0; /* size of the bloody thing! */

  memcpy((char *)&o->tlist[2],(char *)tlist,
                  (o->tllength - 2) * sizeof(struct type_list));

  o->gennames = sds_malloc(strlen(namelist) + 1);
  strcpy(o->gennames, namelist);
  tmp = o->gennames;
  while (*tmp != (char)0)
  {
    if (*tmp ==',')
    {
      *tmp = (char)0;
      ncount++;
    }
    tmp++;
  }
  if (ncount != o->tllength - 5)
  {
    char temp[256];
    sprintf(temp,"Defining object '%s'",name);
    sds_push_error(SDS_NAMELIST,SDS_ERROR,temp);
  }

  n = maketree(o->tlist,
           SDS_INDLIST,
           otree,
           sds_get_top(otree),
           o->gennames,
           1,
           name, 
           SDS_SUN3ARC);

  sds_delete_odef(o);
  sds_start_tree(otree, n);
  sds_makeaddr(stc->odef_tree,0,0);
  return n;
}  

sds_code
sds_nelements(inst)
sds_instance inst;
{
  sds_treecon *stc = sds_treec();
  if (inst->ntree == stc->inst_tree - 1)
  {
    struct sds_inst *i = (struct sds_inst *)inst->data;
    return i->number;
  }
  else
  {
    struct sds_desc * d = (struct sds_desc *)inst->data;
    return d->nelems;
  }
}

char *
sds_object_name(inst)
sds_instance inst;
{
  sds_treecon *stc = sds_treec();
  if (inst->ntree == stc->inst_tree - 1)
  {
     struct sds_inst *i = (struct sds_inst *)inst->data;
     return i->name;
  }
  else
  {
    struct sds_desc *d = (struct sds_desc *)inst->data;
    return d->name;
  }
}

char *
sds_type_name(inst)
sds_instance inst;
{
  struct sds_desc *d;
  sds_treecon *stc = sds_treec();
  if (inst->ntree == stc->inst_tree - 1)
  {
     struct sds_inst *i = (struct sds_inst *)inst->data;
     d = (struct sds_desc *)i->def->data;
  }
  else
    d = (struct sds_desc *)inst->data;
  return d->name;
}

sds_code
sds_type_code(inst)
sds_instance inst;
{
  struct sds_desc *d;
  sds_treecon *stc = sds_treec();
  if (inst->ntree == stc->inst_tree - 1)
  {
     struct sds_inst *i = (struct sds_inst *)inst->data;
     d = (struct sds_desc *)i->def->data;
  }
  else
    d = (struct sds_desc *)inst->data;
  return d->type;
}
