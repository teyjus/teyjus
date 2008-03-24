//////////////////////////////////////////////////////////////////////////////
//Copyright 2008
//  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Zach Snow
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
#include "file.h"
#include "ld_message.h"
#include "../simulator/dataformats.h"
#include "../system/error.h"
#include "../system/memory.h"
#include "../tables/pervasives.h"
#include "../tables/pervinit.h"
#include "loader.h"
#include "tyskel.h"
#include "kind.h"

// #define ARROW 0
// #define PERVASIVE 1
// #define LOCAL 2
// #define GLOBAL 3
// #define VARIABLE 4

typedef enum LD_TYSKEL_SymType{
  ARROW=0,
  KIND=1,
  VARIABLE=2
}LD_TYSKEL_SymType;

void LD_TYSKEL_LoadType(MEM_GmtEnt* ent,MemPtr loc);

int LD_TYSKEL_LoadTst(MEM_GmtEnt* ent)
{
  MemPtr* tst;
  int i;
  TwoBytes tstSize=LD_FILE_GET2();
  LD_detail("Loading %d type skeletons\n",tstSize);
  
  /* MemPtr* tst=
     (MemPtr*)LD_LOADER_ExtendModSpace(ent,(tstSize+PERV_TY_SKEL_NUM)*sizeof(MemPtr)); 
     -- XQ
  */
  tst =(MemPtr*)LD_LOADER_ExtendModSpace(ent,
                                        (tstSize+PERV_TY_SKEL_NUM) *
                                        MEM_TST_ENTRY_SIZE);
  
  ent->tstBase=(MEM_TstPtr)tst;
  //Copy pervasives
  PERVINIT_copyTySkelTab((MEM_TstPtr)tst);
  tst+=PERV_TY_SKEL_NUM;
  
  for(i=0;i<tstSize;i++)
  {
    LD_debug("Loading a type skeleton\n");
    tst[i]=(MemPtr)LD_LOADER_ExtendModSpace(ent,DF_TY_ATOMIC_SIZE);
    LD_TYSKEL_LoadType(ent,tst[i]);
  }
  return 0;
}

/**
\brief Load a type skeleton
\arg loc Where the head of the type skeleton is to be placed.
**/
void LD_TYSKEL_LoadType(MEM_GmtEnt* ent,MemPtr loc)
{
  int i,ind, arity;
  MemPtr args;
  Byte type=LD_FILE_GET1();
  switch(type)
  {
    case ARROW:
      args = (MemPtr)LD_LOADER_ExtendModSpace(ent,2*DF_TY_ATOMIC_SIZE);
      DF_mkArrowType(loc,(DF_TypePtr)args);
      LD_TYSKEL_LoadType(ent,args);
      LD_TYSKEL_LoadType(ent,args+DF_TY_ATOMIC_SIZE);
      break;
    
    case KIND:
      ind = LD_KIND_GetKindInd();
      arity = LD_FILE_GET1();
      if(arity==0)
      {
        DF_mkSortType(loc,ind);
      }
      else
      {
        args = (MemPtr)LD_LOADER_ExtendModSpace(ent,(1+arity)*DF_TY_ATOMIC_SIZE);
        DF_mkStrType(loc,(DF_TypePtr)args);
        DF_mkStrFuncType(loc, ind, arity);
        for(i=1;i<=arity;i++)
          LD_TYSKEL_LoadType(ent,args+i*DF_TY_ATOMIC_SIZE);
      }
      break;
    
    case VARIABLE:
      DF_mkSkelVarType(loc,(int)LD_FILE_GET1());
      break;
      
    default:
      LD_error("Unexpected type skeleton prefix %d.\n",type);
      EM_THROW(LD_LoadError);
  }
}


