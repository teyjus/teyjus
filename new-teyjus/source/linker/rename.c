#include <stdio.h>
#include <stdlib.h>
#include "module.h"
#include "rename.h"
#include "vector.h"
#include "tree.h"
#include "file.h"

void* ConstRNTree=NULL;

void LK_RENAME_LoadConstRNTabEnt();

void LK_RENAME_LoadConstRNTable()
{
  LK_TREE_Empty(&ConstRNTree);
  int size=(int)GET2();
  int i;
  for(i=0;i<size;i++)
  {
    LK_RENAME_LoadConstRNTabEnt();
  }
}

void LK_RENAME_LoadConstRNTabEnt()
{
  Name name;
  GetName(&name);
  MarkInd ind=GetConstInd();
  LK_TREE_Add(&ConstRNTree,name.string,ind);
  Clear(name);
}

ConstInd LK_RENAME_RenameConst(Name name)
{
  return LK_TREE_Retrieve(&ConstRNTree,name.string);
}

void* KindRNTree=NULL;

void LK_RENAME_LoadKindRNTabEnt(int fd ,struct Module_st* CMData);

void LK_RENAME_LoadKindRNTable(int fd ,struct Module_st* CMData)
{
  if(KindRNTree!=NULL)
    LK_TREE_Empty(&KindRNTree);
  int size=(int)LK_FILE_GET2(fd);
  int i;
  for(i=0;i<size;i++)
  {
    LK_RENAME_LoadKindRNTabEnt(fd,CMData);
  }
}

void LK_RENAME_LoadKindRNTabEnt(int fd ,struct Module_st* CMData)
{
  char* name=LK_FILE_GetString(fd);
  MarkInd ind=GetKindInd(fd,CMData);
  LK_TREE_Add(&KindRNTree,name,ind);
  free(name);
}

KindInd LK_RENAME_RenameKind(char* name)
{
  return LK_TREE_Retrieve(&KindRNTree,name);
}
