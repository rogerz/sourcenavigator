#ifndef sds_lisd_h
#define sds_lisd_h 1

#include "Sds/sds_list.h"

  struct type_list nodec_tl[] = 
    {
      {(unsigned long)1, SDS_INTERNAL_POINTER},
      {(unsigned long)1, SDS_INTERNAL_POINTER},
      {(unsigned long)1, SDS_INTERNAL_POINTER},
      {(unsigned long)1, SDS_INTERNAL_POINTER},
      {(unsigned long)1, SDS_INTERNAL_POINTER},
      {(unsigned long)1, SDS_INTERNAL_POINTER},
      {(unsigned long)1, SDS_LONG},
      {(unsigned long)1, SDS_LONG},
      {(unsigned long)1, SDS_LONG},
      {(unsigned long)1, SDS_LONG},
      {(unsigned long)1, SDS_LONG},
      {(unsigned long)1, SDS_CHAR},
      {(unsigned long)1, SDS_SHORT},
      {(unsigned long)0,SDS_RETLIST},
      {(unsigned long)0,SDS_ENDLIST},
    };

  char nodec_names[] = 
  "next->Nodes,prev->Nodes,up,down->Lists,name->_nameh,data,datasize,sds,object,datatype,nelems,align,control";

  struct type_list listc_tl[] = 
    {
      {(unsigned long)1, SDS_INTERNAL_POINTER},
      {(unsigned long)1, SDS_INTERNAL_POINTER},
      {(unsigned long)1, SDS_LONG},
      {(unsigned long)1, SDS_LONG},
      {(unsigned long)1, SDS_INTERNAL_POINTER},
      {(unsigned long)1, SDS_INTERNAL_POINTER},
      {(unsigned long)1, SDS_LONG},
      {(unsigned long)1, SDS_LONG},
      {(unsigned long)1, SDS_UNS_LONG},
      {(unsigned long)1, SDS_LONG},
      {(unsigned long)1, SDS_LONG},
      {(unsigned long)1, SDS_UNS_LONG},
      {(unsigned long)1, SDS_UNS_SHORT},
      {(unsigned long)0,SDS_RETLIST},
      {(unsigned long)0,SDS_ENDLIST},
    };

   char listc_names[] = 
  "head->Nodes,tail->Nodes,nnodes,listid,name->_nameh,databank,banksize,bankused,banktype,sds,object,datatype,control";

#endif
