/* This file contains memory allocation and deallocation
 * functions required to link to libdb.a. A special
 * implementation of these functions is used when
 * TCL_MEM_DEBUG is defined. This code is only linked
 * into programs that include Tcl.
 */

#ifdef TCL_MEM_DEBUG
# include <tcl.h>
#else
# include <stdlib.h>
#endif

void *db_malloc(size_t size) {
#ifdef TCL_MEM_DEBUG
    return ckalloc(size);
#else
    return malloc(size);
#endif
}

void  db_free(void *ptr) {
#ifdef TCL_MEM_DEBUG
    ckfree(ptr);
#else
    free(ptr);
#endif
}

void *db_realloc(void *ptr, size_t size) {
#ifdef TCL_MEM_DEBUG
    return ckrealloc(ptr, size);
#else
    return realloc(ptr, size);
#endif
}

void *db_calloc(size_t nmemb, size_t size) {
#ifdef TCL_MEM_DEBUG
    void *ptr = ckalloc(nmemb * size);
    memset(ptr, 0, nmemb * size);
    return ptr;
#else
    return calloc(nmemb, size);
#endif
}
