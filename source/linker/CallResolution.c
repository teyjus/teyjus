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
#include "datatypes.h"
#include "vector.h"
#include "file.h"
#include "module.h"
#include "hashtab.h"
#include "code.h"
#include "CallResolution.h"

typedef struct{
  ConstInd index;
  int dynamic_flag;
  struct Vector predCalls;
}PredInfo;

typedef struct{
  int arity;
  CodeInd addr;
}PCallEnt;

void InitInfoTab(PredInfoTab* Pit)
{
  LK_VECTOR_Init(Pit,16,sizeof(PredInfo));
}

PredInfo* FindPredInfo(PredInfoTab* Pit, ConstInd index)
{
  int i;
  PredInfo* tmp=NULL;
  int size=LK_VECTOR_Size(Pit);
  if(size>0)
  {
    tmp=(PredInfo *)LK_VECTOR_GetPtr(Pit,0);
    for(i=0;i<size;i++)
    {
      if((tmp[i].index.gl_flag==index.gl_flag)&&(tmp[i].index.index==index.index))
        return tmp+i;
    }
  }
  tmp=(PredInfo *)LK_VECTOR_GetPtr(Pit,LK_VECTOR_Grow(Pit,1));
  tmp->index=index;
  tmp->dynamic_flag=0;
  LK_VECTOR_Init(&(tmp->predCalls),8,sizeof(PCallEnt));
  return tmp;
}

void PushCall(PredInfoTab* Pit, ConstInd index,CodeInd addr,int arity)
{
  PredInfo* PInfo = FindPredInfo(Pit,index);
  PCallEnt* tmp = (PCallEnt*)LK_VECTOR_GetPtr(&(PInfo->predCalls),LK_VECTOR_Grow(&(PInfo->predCalls),1));
  tmp->addr=addr;
  tmp->arity=arity;
}

void MarkDynamic(PredInfoTab* Pit, ConstInd index)
{
  PredInfo* PInfo = FindPredInfo(Pit,index);
  PInfo->dynamic_flag++;
}

void ResolvePredCall(PredInfo* PInfo,HashTab_t* PredSearchTab)
{
  int i;
  PCallEnt* tmp;
  CodeInd addr;
  int size=LK_VECTOR_Size(&(PInfo->predCalls));
  if(size==0)
  {
    LK_VECTOR_Free(&(PInfo->predCalls));
    return;
  }
  tmp=(PCallEnt *)LK_VECTOR_GetPtr(&(PInfo->predCalls),0);
  if(PInfo->dynamic_flag>0)
  {
    for(i=0;i<size;i++)
    {
      MakeCallName(tmp[i].addr,tmp[i].arity,PInfo->index);
    }
  }
  else
  {
    addr=HashCodeAddr(PredSearchTab,PInfo->index);
    for(i=0;i<size;i++)
    {
      MakeCall(tmp[i].addr,tmp[i].arity,addr);
    }
  }
  LK_VECTOR_Free(&(PInfo->predCalls));
}

void ResolvePredCalls(PredInfoTab* Pit,HashTab_t* PredSearchTab)
{
  int i;
  int size=LK_VECTOR_Size(Pit);
  PredInfo* tmp=(PredInfo*)LK_VECTOR_GetPtr(Pit,0);
  for(i=0;i<size;i++)
  {
    ResolvePredCall(tmp+i,PredSearchTab);
  }
  LK_VECTOR_Free(Pit);
}

