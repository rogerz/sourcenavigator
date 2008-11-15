/* $Header$ */

#ifndef ISTKsds_object_h
#define ISTKsds_object_h

#include "Sds/sds_tree.h"

struct sds_odef 
{
  struct type_list *tlist;
  int tllength;
	char *gennames;
	unsigned int tlist_malloced : 1;
};

struct sds_ohandle
{
	struct sds_odef *def;
	struct direc *odptr;
};

typedef struct sds_ohandle * sds_ohandle;
typedef struct sds_odef  * sds_odef;
typedef sds_tnode * sds_defined;
typedef sds_tnode * sds_instance;

struct sds_inst
{
  char      *name;
  sds_code   number;
  sds_code   code;
  void      *pointer;
  sds_defined def;
  unsigned was_allocated:1;
  char refcount;
};

typedef struct sds_inst  sds_inst;

#if defined (__STDC__)
sds_instance sds_instantiate(sds_defined,char *, int, void *);
sds_defined sds_def_object(char *, struct type_list *, char *);

sds_code           sds_nelements(sds_instance);
char             * sds_object_name(sds_instance);
char             * sds_type_name(sds_instance);
sds_code           sds_type_code(sds_instance);
sds_inst         * sds_new_inst(sds_code, char*, sds_code);

#else
sds_instance sds_instantiate();
sds_defined sds_def_object();

sds_code           sds_nelements();
char             * sds_object_name();
char             * sds_type_name();
sds_code           sds_type_code();
sds_inst         * sds_new_inst();

#endif

#endif
