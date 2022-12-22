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
#include <stdio.h>
#include <stdlib.h>
#include "module.h"
#include "rename.h"
#include "vector.h"
#include "tree.h"
#include "file.h"
#include "linker_message.h"

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
  int size;
  int i;

  LK_TREE_Empty(&ConstRNTree);
  size=LK_FILE_GET2(fd);
  
  for(i=0;i<size;i++)
  {
    LK_RENAME_LoadConstRNTabEnt(fd,CMData);
  }
}

ConstInd LK_RENAME_RenameConst(char* name)
{
  ConstInd result;
  EM_TRY{
    result = LK_TREE_Retrieve(&ConstRNTree,name);
  } EM_CATCH {
    bad("Error renaming const \"%s\"\n",name);
    EM_RETHROW();
  }
  return result;
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
  int size;
  int i;

  if(KindRNTree!=NULL)
    LK_TREE_Empty(&KindRNTree);
  size=(int)LK_FILE_GET2(fd);
  
  for(i=0;i<size;i++)
    LK_RENAME_LoadKindRNTabEnt(fd,CMData);
}

KindInd LK_RENAME_RenameKind(char* name)
{
  KindInd result;
  EM_TRY{
    result = LK_TREE_Retrieve(&KindRNTree,name);
  } EM_CATCH {
    bad("Error renaming const \"%s\"\n",name);
    EM_RETHROW();
  }
  return result;
}
