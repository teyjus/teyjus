#include <stdio.h>
#include <stdlib.h>
#include "module.h"
#include "rename.h"
#include "vector.h"
#include "tree.h"
#include "file.h"
#include "message.h"

void* ConstRNTree=NULL;

void LK_RENAME_LoadConstRNTabEnt(int fd ,struct Module_st* CMData)
{
  char* tmp=LK_FILE_GetString(fd);
  MarkInd ind=GetConstInd(fd,CMData);
  LK_TREE_Add(&ConstRNTree,tmp,ind);
  free(tmp);
}

void LK_RENAME_LoadConstRNTable(int fd ,struct Module_st* CMData)
{
  LK_TREE_Empty(&ConstRNTree);
  int size=LK_FILE_GET2(fd);
  int i;
  for(i=0;i<size;i++)
  {
    LK_RENAME_LoadConstRNTabEnt(fd,CMData);
  }
}

ConstInd LK_RENAME_RenameConst(char* name)
{
  EM_TRY{
    return LK_TREE_Retrieve(&ConstRNTree,name);
  } EM_CATCH {
    bad("Error renaming const \"%s\"\n",name);
    EM_RETHROW();
  }
}

void* KindRNTree=NULL;

void LK_RENAME_LoadKindRNTabEnt(int fd ,struct Module_st* CMData)
{
  char* name=LK_FILE_GetString(fd);
  MarkInd ind=GetKindInd(fd,CMData);
  LK_TREE_Add(&KindRNTree,name,ind);
  free(name);
}

void LK_RENAME_LoadKindRNTable(int fd ,struct Module_st* CMData)
{
  if(KindRNTree!=NULL)
    LK_TREE_Empty(&KindRNTree);
  int size=(int)LK_FILE_GET2(fd);
  int i;
  for(i=0;i<size;i++)
    LK_RENAME_LoadKindRNTabEnt(fd,CMData);
}

KindInd LK_RENAME_RenameKind(char* name)
{EM_TRY{
  return LK_TREE_Retrieve(&KindRNTree,name);
} EM_CATCH {
  bad("Error renaming const \"%s\"\n",name);
  EM_RETHROW();
}
}
