/* This file contains memory allocation and deallocation
 * functions used in the rest of the bdb code. Most code
 * will add db_malloc.o to the link line. Some programs
 * may want to use a debug set of alloc routines, those
 * programs should substitue another implementation of
 * these routines.
 */

#include <stdlib.h>

void *db_malloc(size_t size) {
    return malloc(size);
}

void  db_free(void *ptr) {
    free(ptr);
}

void *db_realloc(void *ptr, size_t size) {
    return realloc(ptr, size);
}

void *db_calloc(size_t nmemb, size_t size) {
    return calloc(nmemb, size);
}

