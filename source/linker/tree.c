#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../system/error.h"
#include "tree.h"
#include "datatypes.h"

typedef struct{
  char* key;
  MarkInd data;
}entry;

int compar_fn(const void* pa,const void* pb)
{
  const char* na = ((const entry*) pa) -> key;
  const char* nb = ((const entry*) pb) -> key;
  return strcmp(na,nb);
}

void free_node(void* p)
{
  free(((entry*)p)->key);
  free(p);
}

void LK_TREE_Add(void** root, char* key, MarkInd ind)
{
  entry* p=(entry*)EM_malloc(sizeof(entry));
  p->key=EM_malloc(strlen(key)+1);
  strcpy(key,p->key);
  p->data=ind;
  if(NULL==tsearch((void*)p,root,compar_fn))
    EM_THROW(EM_OUT_OF_MEMORY);
}

MarkInd LK_TREE_Retrieve(void **root, char* key)
{
  entry e;
  e.key=key;
  entry* p=(entry*)tfind((void*)&e,root,compar_fn);
  if(NULL==p)
    EM_THROW(LK_LinkError);
  return p->data;
}

void LK_TREE_Empty(void **root)
{
  tdestroy(root,free_node);
  *root=NULL;
}
