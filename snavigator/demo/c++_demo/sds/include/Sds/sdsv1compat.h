#if !defined(sdsv1compat_h)
#define sdsv1compat_h 1



/*  These calls are wrappers to preserve the error returns of V1... */

EXTERN sds_handle sds_define_object(sds_handle,struct type_list *,char *);
EXTERN sds_handle sds_declare_object(sds_handle, void *,
                                               char *,sds_code,sds_code);
EXTERN sds_handle     sds_load_fd(int, sds_code , int);
EXTERN int            write_sds2socket(int , sds_handle );
EXTERN sds_handle     sds_dataset_size(sds_handle);
EXTERN int            read_socket2sds(int , int );
EXTERN sds_handle     sds_duplicate(sds_handle, char *);
EXTERN sds_handle     sds_new_index(char *);
EXTERN sds_handle     sds_use(char *,sds_code, sds_code);
EXTERN sds_handle     sds_assemble(sds_handle,char *,sds_code);
EXTERN sds_handle     sds_obname2ind(sds_handle ,char *);
EXTERN sds_handle     sds_oblike2ind(sds_handle ,char *, sds_handle);

#endif

