#ifndef sds_tree_h
#define sds_tree_h

#define NTREES 16
#define SDS_TYPELIST SDS_RETLIST
#define SDS_REAL_ADDRESS    (char)0
#define SDS_OFFSET_ADDRESS  (char)1

#define SDS_TREE_DEFN_DATA 1
#define SDS_TREE_INST_DATA 2
#define SDS_TREE_ADD_DATA 3

struct sds_tnode {
  struct sds_tnode *next;
  struct sds_tnode *down;
  struct sds_tnode *top;
  struct sds_tnode *head;
  int ntree;
  int datatype;
  void *data;
  };

struct tree_pos {
  short width;
  short depth;
  int ntnodes;
  int flag;
  struct sds_tnode *top;
};

struct sds_desc 
{
  sds_code  nelems;
  char     *name;
  sds_code  type;
  sds_code  size;
  sds_code  ssize;
  long      offset;
  long      soffset;
  char      align;
	char      salign;
  char      refcount;
  sds_code  mark;
};


typedef  struct sds_tnode sds_tnode;
typedef  struct tree_pos tree_pos;
typedef  struct sds_desc sds_desc;
typedef  struct sds_treecon sds_treecon;

struct sds_treecon 
{
  int next_tree;
  sds_tnode **top;
  sds_tnode **trav;
  tree_pos *tpos;
	int odef_tree;
  int inst_tree;
};

#if defined (__STDC__)
void              sds_makeaddr(int,void *, void *);
sds_tnode        *maketree( struct type_list *,sds_code  ,int,
                       sds_tnode *,char *, sds_code, char *, int);
sds_tnode        * sds_down_tnode(sds_tnode *, void *);
sds_tnode        * sds_next_tnode(sds_tnode *, void *);
sds_tnode        * sds_new_tnode(void *);
void              sds_start_tree(int, sds_tnode *);
sds_tnode        * sds_trav_tree(int);
sds_tnode        * sds_res(int);
sds_tnode        * sds_get_top(int);
int               sds_new_tree(void *);
tree_pos        * sds_get_pos(int);
void              sds_delete_tree(int,sds_tnode *,int);
int               sds_tree(sds_handle, sds_handle,int);
void              sds_print_tree(int, int);
sds_tnode        * sds_findname(int, char *);
sds_tnode        * sds_likename(int, sds_tnode *, char *);
sds_tnode        * sds_check_sizes(int);
sds_desc         * sds_new_desc(sds_code, char*, sds_code);


#else /* Not ansi-C */
sds_tnode        *sds_makeaddr();
void              maketree();
sds_tnode        * sds_down_tnode();
sds_tnode        * sds_next_tnode();
sds_tnode        * sds_new_tnode();
void              sds_start_tree();
sds_tnode        * sds_trav_tree();
sds_tnode        * sds_res();
sds_tnode        * sds_get_top();
int               sds_new_tree();
tree_pos        * sds_get_pos();
void              sds_delete_tree();
int               sds_tree();
void              sds_print_tree();
sds_tnode        * sds_findname();
sds_tnode        * sds_likename();
sds_tnode        * sds_check_sizes();
sds_desc         * sds_new_desc();


#endif /* Ansi C or not ansi C */

#endif
