

/* Wrappers for compatibility V1 -> V2   */

#include "Sds/sdsgen.h"

/*********************************************************************/
sds_handle 
sds_define_object(sds,ty_list,names)
sds_handle  sds;
struct type_list *ty_list;
char  *names;
/*********************************************************************/
{
  sds_handle code;
  if (!(code = sds_define_structure(sds,ty_list,names)))
    return sds_last_error();
  return code;
}
/*********************************************************************/
sds_handle
sds_declare_object(sds,obj_ptr,name,number,code)
sds_handle sds;
void  *obj_ptr;
char  *name;
sds_code number;
sds_code code;
/*********************************************************************/
{
  sds_handle tcode;
  if (!(tcode = sds_declare_structure(sds,obj_ptr,name,number,code)))
    return sds_last_error();
  return tcode;
}
/***********************************************************************/
int 
write_sds2socket(fd,sds)
int fd;
sds_handle sds;
/***********************************************************************/
{
  sds_handle code;
  if (!(code = sds_write2fd(fd,sds))) return sds_last_error();
  return code;
}
/*********************************************************************/
sds_handle
sds_dataset_size(sds)
sds_handle sds;
/*********************************************************************/
{
  sds_handle code;
  if (!(code = sds_fullsize(sds))) return sds_last_error();
  return code;
}
/*********************************************************************/
int
sds_read_socket2sds(fd,mode)
int fd;
int mode;
{
  sds_handle code;
  if (!(code = sds_read_open_fd(fd,0))) return sds_last_error();
  return code;
}
/*********************************************************************/
sds_handle
sds_duplicate(old_sds,name)
sds_handle  old_sds;
char *name;
{
  sds_handle code;
  if (!(code = sds_dup(old_sds,name))) return sds_last_error();
  return code;
}
/*********************************************************************/
sds_handle
sds_new_index(name)
char *name;
{
  sds_handle code;
  if (!(code = sds_new(name))) return sds_last_error();
  return code;
}
/*********************************************************************/
sds_handle
sds_use(source_name,source_type,mode)
char *source_name;
sds_code source_type,mode;
{
  sds_handle code;
  if (!(code = sds_access(source_name,source_type,mode))) return sds_last_error();
  return code;
}
/*********************************************************************/
sds_handle
sds_assemble(sds,name,flg)
sds_handle sds;
sds_code flg;
char  *name;
{
  sds_handle code;
  if (!(code = sds_ass(sds,name,flg))) return sds_last_error();
  return code;
}
/***********************************************************************/
/*          Version 1 wrapper ***********/
sds_handle 
sds_obname2ind(sds,name)
sds_handle  sds;
char  *name;
{
  sds_handle  code;
  if (!(code = sds_name2ind(sds,name))) return sds_last_error();
  return code;
}
/***********************************************************************/
/*          Version 1 wrapper ***********/
sds_handle 
sds_oblike2ind(sds,name,start)
sds_handle  sds,start;
char  *name;
{
  sds_handle  code;
  if (!(code = sds_like2ind(sds,name,start))) return sds_last_error();
  return code;
}
/***********************************************************************/
/*          Version 1 wrapper ***********/
sds_handle 
is_sds(name)
char *name;
{
  sds_handle sds;
  if ((sds = good_sds(name)))
    return sds;
  else
    return SDS_NO_SUCH_SDS;
}
