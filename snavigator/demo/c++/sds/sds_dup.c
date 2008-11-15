/* $Header$ */

#include "Sds/sdsgen.h"
#include "Sds/sds_externs.h"
#ifndef VXWORKS
#include "malloc.h"
#endif

#if defined (__STDC__)
sds_handle sds_type_duplicate_def(sds_handle, sds_handle, sds_code);
#else
sds_handle sds_type_duplicate_def();
#endif


/*********************************************************************/
sds_handle
sds_duplicate_object(new_sds,old_sds,object)
sds_handle  new_sds,old_sds,object;
/*********************************************************************/
{
  struct  direc  *dptr,*old_dptr;
  sds_code typecode;
  sds_code newtypecode;
	 sds_handle newobject;
		struct sds_control_p *scp = sds_control(new_sds);

  if (!sds_initialised()) return 0L;

  if ((old_dptr = sds_direc(old_sds)) == DNULL ||
      (dptr = sds_direc(new_sds)) == DNULL)
  {
    sds_push_error(SDS_NO_SUCH_SDS,SDS_ERROR,"duplicating object");
    return 0L;
  }

  if (object == 0 || object > old_dptr[0].nelems)
  {
    sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"duplicating object");
    return 0L;
  }

  if (scp->dup_size == NULL)
   scp->dup_size = (sds_handle *)sds_malloc((dptr[0].nelems + 1) * sizeof(int));
  else
   scp->dup_size = (sds_handle *)sds_realloc((char *)scp->dup_size,
																(dptr[0].nelems + 1) * sizeof(int));
  sds_cleanup(old_sds);
  if ((typecode = old_dptr[object].elemcod) & SDS_INDLIST)
	{
   newtypecode = sds_type_duplicate_def(old_sds,new_sds,typecode);
		 typecode = newtypecode;
	}

  newobject = sds_declare_structure(new_sds, 
    sds_obind2ptr(old_sds,object),
    sds_obind2name(old_sds,object),
    old_dptr[object].nelems,
    typecode);

  if (newobject)
	{
    scp->dup_size[newobject] = old_dptr[object].nelems;

     dptr[newobject].structype = old_dptr[object].structype;  
     if (old_dptr[object].illoca == SDS_DISJOINT_OBJECT)
       dptr[newobject].illoca = SDS_DISJOINT_OBJECT;  
	}
  return newobject;
}
/*********************************************************************/
sds_handle
sds_type_duplicate_def(source_sds_index,new_sds_index,typecode)
sds_handle  source_sds_index,new_sds_index;
sds_code typecode;
/*********************************************************************/
{
  char              *temp,*name_source,*namelist;
  struct  type_list *tlist_source,*tlist;
  int                nnames;
  int                tlist_counter = 1;
  int                name_counter = 0;
  int                char_counter = 0;
  int                counter;
  sds_code           tc;

  if (typecode < (long)NTYPES) /* It's a primitive */
    return typecode;
  tlist_source = sds_tlist(source_sds_index);
  if (tlist_source == NULL)
		{
				sds_push_error(SDS_NO_SUCH_OBJ,SDS_ERROR,"Duplcating type list");
				return 0L;
		}
  tlist_source += typecode & ~SDS_CODE_MASK;
  temp = 
  name_source = sds_heap(source_sds_index) + (tlist_source[0].nelems & 0xffff);
  nnames = tlist_source[0].nelems >> 16;

  while (name_counter++ < nnames)
    while (name_source[char_counter++] != 0);

  namelist = sds_malloc(char_counter + 1);

  if (namelist == 0 )
  {
    sds_perror("malloc failure, sds_transfer_object");
    exit(1);
  }

  for (counter = 0;counter < char_counter; counter++)
  {
    namelist[counter] = temp[counter];
    if (namelist[counter] == 0)
      namelist[counter] = ',';
  }
  namelist[counter] = 0;

  while (~tlist_source[tlist_counter++].elemcod & SDS_RETLIST);
  tlist_counter--;

  tlist = (struct type_list *)sds_malloc(tlist_counter * sizeof(struct type_list));
  tlist_source += 2;
  memcpy((char *)tlist,(char *)tlist_source,
      (tlist_counter - 1) * sizeof(struct type_list));
  tlist[tlist_counter - 1].elemcod = SDS_ENDLIST;
  tlist[tlist_counter - 1].nelems = 0;

  counter=0;
  while (!((tc = tlist[counter].elemcod) & SDS_RETLIST))
  {
    if (tc & SDS_INDLIST )
      tlist[counter].elemcod = 
            sds_type_duplicate_def(source_sds_index,new_sds_index,tc);
    counter++;
  }

  typecode = sds_define_structure(new_sds_index,tlist,namelist);

  free(namelist);
  free((char *)tlist);

  return typecode;
}
