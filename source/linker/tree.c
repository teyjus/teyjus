//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Nathan Guermond, Steven Holte, 
//  Gopalan Nadathur, Xiaochu Qi, Zach Snow
//////////////////////////////////////////////////////////////////////////////
// This file is part of Teyjus.                                             //
//                                                                          //
// Teyjus is free software: you can redistribute it and/or modify           //
// it under the terms of the GNU General Public License as published by     //
// the Free Software Foundation, either version 3 of the License, or        //
// (at your option) any later version.                                      //
//                                                                          //
// Teyjus is distributed in the hope that it will be useful,                //
// but WITHOUT ANY WARRANTY; without even the implied warranty of           //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
// GNU General Public License for more details.                             //
//                                                                          //
// You should have received a copy of the GNU General Public License        //
// along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.          //
//////////////////////////////////////////////////////////////////////////////
#include <stdlib.h>
#include <string.h>
#include "../include/standardlib.h"
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
  p->key=(char *)EM_malloc(strlen(key)+1);
  strcpy(p->key,key);
  p->data=ind;
  if(!tsearch((void*)p,root,compar_fn))
  {
    EM_THROW(EM_OUT_OF_MEMORY);
  }
}

MarkInd LK_TREE_Retrieve(void **root, char* key)
{
  entry** p;
  entry e;

  e.key=key;
  e.data.gl_flag=3;

  p=(entry**)tfind((void*)&e,root,compar_fn);
  if(NULL==p){
    EM_THROW(LK_LinkError);
  }
  return (*p)->data;
}

void LK_TREE_Empty(void **root)
{
  tdestroy(*root,free_node);
  *root=NULL;
}
