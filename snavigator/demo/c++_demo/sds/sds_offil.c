/* $Header$ */

#if !defined(VXWORKS) && !defined(vms)
#include "malloc.h"
#include <memory.h>
#endif

#if defined(vms)
#include "sdsgen.h"
#include "sds_externs.h"
#else
#include "Sds/sdsgen.h"
#include "Sds/sds_externs.h"
#endif

extern char  *strstr();

/************* forward declarations */
int CheckVarLength(sds_handle ,sds_handle,char **,int);
int FillBaseAddress(sds_handle ,sds_handle,char **,int);
int ScanFortranFile(sds_handle ,sds_handle,char **,int);
int LoadFromHeader(sds_handle ,sds_handle,char **,int);
int allocate_data_pointers(sds_handle,sds_handle,int);

/* The arguments passed to offil need some explanation.
   The sds handle is fairly simple, and if this is a normal dataset
   without externally mapped objects ("DISJOINT") the story ends there:
   offsets from the start  of the dataset are calculated from object
   sizes as defined in the header.
   If, however, we have objects mapped in there are two main possiblities:
   1. All data objects come from the same mapped region: that is, the only
      alignment to be calculated differently is that between the header and
      the first data object.
   2. A mixture of objects from the header and from mapped region(s) are
      present; offsets are somewhat more complex.
   (1) is a subset of (2) but likely to be common so the interface is 
   simplified. In the case of (1), the NAddr value is 1, DataAddr points
   to the single mapped region, and DataSize points to the single size
   value (note: if sizes are set to 0, SDS will try to get a rational
   size by stat'ing the file or calculating from header definitions. The
   size parameter is there primarily for when hardware devices need to be
   mapped.)
   In the more complex case where data objects
   may reside with the header, and one or more objects may be in each
   region, NAddr should match the number of objects (if not it will be
   assumed that all unmarked objects run on from the last marked data
   region). DataAddr[0] MUST be nil, as should all DataAddr entries
   referring to objects residing in the header file. Other DataAddr entries
   will point to the data region for the corresponding object EXCEPT if
   sizes[object] is negative; for sizes[object] = -n, the object will be
   assumed to run on from the data region of object 'n'.

   In the case of variable-size data, two strategies are presently
   supprorted. Firstly, explicitly named counts may be present in the data
   objects. In this case, a top-level object <mystruct> may contain an
   integer or short field named <mystruct.tot_avail>: this implies that
   the following object has a multiplicity of the value of <mystruct.tot_avail>.
  (Note: for SDC, the object "Number of Channels" is also searched for)

   Next, a field <mystruct.varstruct_count> within a top level object 
   <mystruct> implies that there is a variable number of structures called
   <mystruct.varstruct at> the end of each instance of <mystruct>.
   The second strategy is that employed by fortran writes. This is more
   limited and compiler dependant so many potholes exist to fall into, but
   is
   probably quite consistent across Unix at least. Here one has a number of
   'records', each of the same structure, contained in 'events'. The
   'events' are delimited at each end by the number of bytes within the
   event, EXCLUDING the two bytecount integers. Thus, knowing the record
   size one can scan a file and split the data hierarchically into a number
   of events each containing a different number of records.
   In both cases, SDS will scan the datafiles and generate a pointer array
   for the top-level objects, each pointer being associated with the number
   of variable things - 'vatstruct's' or 'records' - within each instance
   of the high level thing.When these data address pointer arrays have been
   built, they may be saved with the header information so that scanning is
   not necessary every time - or at least scanning can being where it left
   off last time if, for instance, the data file has been appended to.

 */

/* These statics will hold information about yyyy_count numbers,
   to be matched against yyyy[] arrays if they are found...
 */
static char *count_base;
static int count,precount,presize;
static int countoff;
static int is_short;
static int countbits;
static char *varname = NULL;


sds_handle 
offil(sds, DataAddr, NAddr, DataSize)
sds_handle sds;
char **DataAddr;
int    NAddr;
off_t   *DataSize;
{
  unsigned long    i,lhsize,offset, offsetbase;
  unsigned long    hoffset, doffset = 0;
  struct direc *dptr = sds_direc(sds);
  struct sds_odesc *thing;
  char name[256];
  int next_nelems = -1;
  int topvarlen = 0;
  int osize;
  int NElems = dptr[0].nelems;
  char **DA;
  char **old_addresses;
  off_t *S;
  int allocated;
  int padding;

  if (NAddr > NElems)
  {
    fprintf(stderr,"Number of map addresses exceeds number of objects\n");
    fprintf(stderr,"Bailing out from dataset %s\n",sds_obind2name(sds,0));
    exit(1);
  }
  old_addresses = (char **)sds_malloc(NElems * sizeof(char *));
  for (i=0;i<NElems;i++)
    old_addresses[i] = sds_obind2ptr(sds,i);
  if (NElems > 0 && NAddr != NElems)
  {
    DA = (char **)sds_malloc(NElems * sizeof(char *));
    S = (long *)sds_malloc(NElems * sizeof(int));
    allocated = 1;
    if (NAddr > 0)
    {
      memcpy(DA,DataAddr,NAddr * sizeof(char *));
      memcpy(S,DataSize,NAddr * sizeof(int));
    }
    else
    {
      memset((char *)DA,0,NElems * sizeof(char *));
      memset((char *)S,0,NElems * sizeof(int));
    }
  }
  else
  {
    DA = DataAddr;
    S = DataSize;
    allocated = 0;
  }

  lhsize = (unsigned long )(tlist_size(sds_tlist(sds)) +
            sds_heap_size(sds));
  lhsize += align_delta(lhsize+BASE_OFFSET,sds_palign(SDS_DIRECTORY_STRUCTURE));
     
  dptr[0].elemsz = (unsigned long)sizeof(struct direc);
  dptr[0].elemcod = SDS_DIRECTORY_STRUCTURE;
     
  dptr[0].align_type = sds_palign(SDS_DIRECTORY_STRUCTURE);

  if (dptr[0].structype & SDS_VARIABLE_LENGTH)
    topvarlen = 1;
     
  offsetbase = BASE_OFFSET + lhsize;

  dptr[0].offst = (sds_off_t)offsetbase;

  hoffset = offset = offsetbase + dptr[0].nelems*dptr[0].elemsz;

/* Find my base addresses and sizes */
  for ( i = 1; i< NElems ; i++ ) 
  {
    if (next_nelems > 0)
    {
/* If I have previously found the number of elements, load it in */
      if (dptr[i].structype & SDS_EXTERNAL_LENGTH) 
        dptr[i].nelems = next_nelems;
      /* next_nelems = -1; */
    }

    if (!(dptr[i].illoca & SDS_DISJOINT_OBJECT))
    {
      offset = hoffset;
      padding = align_delta(offset,dptr[i].align_type);
      offset += padding;
      S[i-1] += padding;
      dptr[i].offst = (sds_off_t)offset;
      osize = dptr[i].nelems*dptr[i].elemsz;
      hoffset = offset = (unsigned long)dptr[i].offst + osize;
      S[i] = osize;
      DA[i] = DA[i-1] + S[i-1];
    }
    else
    {
      if (DA[i] != NULL)
      {
        dptr[i].offst = (sds_off_t)offsetbase +
                ((unsigned long)DataAddr[i] - (unsigned long)dptr);
        if ((osize = CheckVarLength(sds,i,old_addresses,S[i])) == -1)
          osize = dptr[i].nelems * dptr[i].elemsz;
        doffset = (long)dptr[i].offst + osize;
        S[i] = osize;
      }
      else if (doffset != 0)
      {
        offset = doffset;
        padding = align_delta(offset,dptr[i].align_type);
        S[i-1] += padding;
        offset += padding;
        dptr[i].offst = (sds_off_t)offset;
        if ((osize = CheckVarLength(sds,i,old_addresses,S[i])) == -1)
          osize = dptr[i].nelems * dptr[i].elemsz;
        DA[i] = DA[i-1] + S[i-1];
        doffset = offset = (long)dptr[i].offst + osize;
        S[i] = osize;
      }
      else
      {
        dptr[i].offst = SDS_IMPOSSIBLE_ADDRESS;
        S[i] = 0;
      }
    }
    if (topvarlen && dptr[i].offst != SDS_IMPOSSIBLE_ADDRESS) 
    {
      char * oaddr = sds_obind2ptr(sds,i);
/* Find the number of elements of the next object */
      if (!strcmp(sds_obind2name(sds,i), "Number of Channels"))
        next_nelems = *(int *)(oaddr);
      else
      {
        sprintf(name,"%s.tot_avail", sds_obind2name(sds, i));
        if (sds_find_thing(sds, name, &thing) >= 0)
           next_nelems = *(int *)(DA[i] + (long)thing->address - (long)oaddr);
        sds_cleanup(sds);
      }
    }
  }

  if (allocated)
  {
    free(DA);
    free(S);
  }
  free(old_addresses);
  return 1L;
}

void
ScanForPreCount(sds,object)
sds_handle sds,object;
{
  struct   sds_odesc *thing;
  struct direc *dptr = sds_direc(sds);
  char *temp;
  int level;

  sds_cleanup(sds);
  while ((level = sds_describe(sds,object,&thing)) >= 0) 
  {
    if (((thing[level].elemcod == SDS_INT) || 
        (thing[level].elemcod == SDS_SHORT)) && 
        (strstr(thing[level].name, "_count")) != NULL)
    {
      count_base = sds_obind2ptr(sds, object);
      countoff = thing[level].address - count_base;
      presize = dptr[object].elemsz;
      if (varname != NULL)
      {
        free(varname);
        varname = NULL;
      }
      varname = sds_malloc(strlen(thing[level].name) + 1);
      strcpy(varname,thing[level].name);
      temp = strstr(varname, "_count");
      if (thing[level].elemcod == SDS_SHORT)
      {
        is_short = 1;
        count = (int)*(short *)thing[level].address;
      }
      else
      {
        is_short = 0;
        count = *(int *)thing[level].address;
      }
      if (strstr(varname,"_countbits") != NULL)
      {
        countbits = 1;
        count = 1 << count;
      }
      else
        countbits = 0;
      *temp = (char )0;
    }
  }
}


int
CheckVarLength(sds,object, addresses, size)
sds_handle sds,object;
char **addresses;
int size;
{
  struct direc *dptr = sds_direc(sds);
 ScanForPreCount(sds,object);
  if (dptr[object].structype & SDS_VARIABLE_LENGTH)
  {
/* I have variable length objects: chose the correct strategy for 
   loading their address and multiplicity counters
 */
    return FillBaseAddress(sds, object, addresses, size);
  }
  else if ((dptr[object].structype & SDS_FORTRAN_RECORDS) ||
           (dptr[object].structype & SDS_SDC_EVENT))
  {
    return ScanFortranFile(sds, object, addresses, size);
  }
  return -1;
}

int
ScanFortranFile(sds, object, addresses, size)
sds_handle sds;
sds_handle object;
char **addresses;
int size;
{
  struct direc *dptr = sds_direc(sds);
  char *base = sds_obind2ptr(sds, object);
  int curplace = 0;
  int bytecount = 0;
  int endcount = 0;
  int nevs = 0;
  int nrecs;
  int recsize;
  char *curaddr = base;
  struct sds_control_p *scp = sds_control(sds);

  if (base == NULL)
  {
/* I'm apparently not attached to data - must be just a template */
    return 0;
  }

  /* Best find out the record size; on Unix this is going to be
     the size of the object less the two ints which are the bytecounts
     (yuk) plus a bit of junk..
   */
  recsize = dptr[object].elemsz - 3 * sizeof(int);

  if ((size = LoadFromHeader(sds,object, addresses, size)) >= 0)
    return(size);

  /* This is the first pass so I really have to scan the datafile */

  /* I'm pointing at the count of bytes IN THE RECORDS */
  bytecount = *(int *)curaddr;
  /* Now I point to first record of first event */
  curplace += (bytecount + 4);

  while (curplace < size)
  {
    curaddr = base + curplace;
    endcount = *(int *)curaddr;
    curplace += 4;
    curaddr += 4;
  /* After the records I expect the same bytecount: check it */
    if (bytecount != endcount)
    {
      fprintf(stderr,"Scan of alleged Fortran record file finds unmatched\n");
      fprintf(stderr,"byte counts between events - bailing out\n");
      exit(1);
    }
    bytecount = *(int *)curaddr;
    nevs++;
    curplace += (bytecount + 4);
  }
  /* Now I know how many events I have I can build the space for the
     actual pointers and mutiplicities
   */
  dptr[object].nelems = nevs;
  allocate_data_pointers(sds,object,nevs);

  curplace = 0;
  bytecount = 0;
  endcount = 0;
  nevs = 0;

  /* Now I scan again to fill in the arrays allocated above */  
  /* I should really do this in one pass. I will when the code
     works.
   */
  while (curplace < size)
  {
    curaddr = base + curplace;
    bytecount = *(int *)curaddr;
    nrecs = bytecount/recsize;
    scp->element_start[object][nevs] = curaddr;
    scp->varel_count[object][nevs] = nrecs;
    nevs++;
    curplace += bytecount + 8;
  }
  /* curplace is now the total number of bytes scanned */
  return curplace;
}

int
allocate_data_pointers(sds,object,nevs)
sds_handle sds,object;
int nevs;
{
  struct direc *dptr = sds_direc(sds);
  struct sds_control_p *scp = sds_control(sds);

  if (scp->element_start == NULL)
  {
    scp->element_start = 
          (char ***)sds_malloc(sizeof(char *) * dptr[0].nelems);
    scp->varel_count = 
          (int **)sds_malloc(sizeof(int) * dptr[0].nelems);
  }
  scp->element_start[object] = 
        (char **)sds_malloc(sizeof(char *) * nevs);
  scp->varel_count[object] =
        (int *)sds_malloc(sizeof(int) * nevs);
  return 1;
}

int
FillBaseAddress(sds, object, addresses, size)
sds_handle sds;
sds_handle object;
char **addresses;
int size;
{
  struct direc *dptr = sds_direc(sds);
  struct type_list *tlist = sds_tlist(sds);
  int obsize = dptr[object].elemsz, varsize;
  struct   sds_odesc *thing;
  int level;
  struct sds_control_p *scp = sds_control(sds);


  int fullsize = 0;
  int i;
  int basesize;
  int foundmatch = 0;
  sds_code varcode;

  char *base = sds_obind2ptr(sds, object);
  if (base == NULL)
  {
/* I'm apparently not attached to data - must be just a template */
    return 0;
  }
  if ((size = LoadFromHeader(sds,object, addresses, size)) >= 0)
    return(size);

  sds_cleanup(sds);
  while ((level = sds_describe(sds,object,&thing)) >= 0) 
  {
    if ((varname != NULL) &&
        (!strcmp(varname,thing[level].name)))
    {
      varsize = thing[level].size;
      varcode = thing[level].ind - 1;
      foundmatch = 1;
    }
  }
  sds_cleanup(sds);
  if (foundmatch)
  {
    if (scp->element_start == NULL)
    {
      scp->element_start = 
            (char ***)sds_malloc(sizeof(char *) * dptr[0].nelems);
      scp->varel_count = 
            (int **)sds_malloc(sizeof(int) * dptr[0].nelems);
    }
    scp->element_start[object] = 
          (char **)sds_malloc(sizeof(char *) * dptr[object].nelems);
    scp->varel_count[object] = 
          (int *)sds_malloc(sizeof(int) * dptr[object].nelems);

    precount = tlist[varcode].nelems;
    tlist[varcode].nelems = SDS_LENGTH_UNDETERMINED;
/* The nelems number in the header may be 'first element', or, more
   likely, is marked UNDETERMINED: so find out the base size from that. */
    if (precount > 0)
      basesize = obsize - varsize * precount;
    else
      basesize = obsize - varsize;


    for (i=0;i<dptr[object].nelems;i++)
    {
      scp->element_start[object][i] = base;
      scp->varel_count[object][i] = count;
      fullsize += (basesize + count * varsize);
      if (i != (dptr[object].nelems - 1))
      {
        if (count_base != base)
          count_base += presize;
        else
          count_base += (basesize + count * varsize);
        base += (basesize + count * varsize);
        if (is_short)
          count = (int)*(short *)(count_base + countoff);
        else
          count = *(int *)(count_base + countoff);
        if (countbits) count = 1 << count;
      }
    }
  }
  else
    fullsize = dptr[object].nelems * dptr[object].elemsz;
  return fullsize;
}

int 
LoadFromHeader(sds,object, addresses, size)
sds_handle sds;
sds_handle object;
char **addresses;
int size;
{

  char buffer[256];
  sds_handle ind;
  struct direc *dptr = sds_direc(sds);
  char *base = sds_obind2ptr(sds, object);
  struct sds_control_p *scp = sds_control(sds);

  /* First check: has this been done before? */
  sprintf(buffer,"%s.Address",sds_obind2name(sds,object));
  if ((ind = sds_name2ind(sds,buffer)) > 0)
  {

  /* Aha! The pointer array has already been saved in the headerSDS */
    int *counts,*offsets, i;
    offsets = (int *)addresses[ind];
  /* Find the counts as well */
    sprintf(buffer,"%s.Mult",sds_obind2name(sds,object));
    ind = sds_like2ind(sds,buffer,1);
    counts = (int *)addresses[ind];
  /* allocate pointer and multiplicity arrays */
    allocate_data_pointers(sds,object,dptr[object].nelems);
  /* And fill them with a little bit of arithmetic */
    for (i=0;i<dptr[object].nelems;i++)
    {
      scp->element_start[object][i] = base + offsets[i] - offsets[0];
      scp->varel_count[object][i] = counts[i];
    }
  /* return total size scanned */
  return size;
  }
  return -1;
}
