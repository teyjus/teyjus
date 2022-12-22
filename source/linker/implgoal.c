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
#include "vector.h"
#include "file.h"
#include "module.h"
#include "datatypes.h"
#include "hashtab.h"
#include "VectorRW.h"
//////////////////////////////////////////////////////
//ImplGoal Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
  TwoBytes nctSize;
  ConstInd* extPred;
  Byte findCodeFun;
  HashTab_t findCodeTab;
}TImplGoal_t;

typedef struct{
  ConstInd index;
  CodeInd addr;
}HashTabEnt;

struct Vector ImplGoals;

void InitTImplGoals()
{
  LK_VECTOR_Init(&ImplGoals,32,sizeof(TImplGoal_t));
}

void LoadImplGoal(int fd, struct Module_st* CMData,void* entry)
{
  int i;
  TImplGoal_t* ImplGoal=(TImplGoal_t*)entry;
  TwoBytes nctSize = ImplGoal->nctSize=LK_FILE_GET2(fd);
  ConstInd* tmp=ImplGoal->extPred=(ConstInd *)EM_malloc(nctSize*sizeof(ConstInd));
  for(i=0;i<nctSize;i++)
  {
    tmp[i]=GetConstInd(fd,CMData);
    MarkDynamic(CMData->Pit,tmp[i]);
  }
  
  ImplGoal->findCodeFun=LK_FILE_GET1(fd);
  
  LoadHashTab(fd,CMData,&(ImplGoal->findCodeTab));
}

void LoadImplGoals(int fd, struct Module_st* CMData)
{
  LK_VECTOR_Read(fd,&ImplGoals,CMData,&(CMData->ImplGoalAdj),LoadImplGoal);
}

void WriteImplGoal(int fd, void* entry)
{
  int i;
  TImplGoal_t* ImplGoal=(TImplGoal_t*)entry;
  TwoBytes nctSize=ImplGoal->nctSize;
  LK_FILE_PUT2(fd,nctSize);
  for(i=0;i<nctSize;i++)
  {
    PutConstInd(fd,(ImplGoal->extPred)[i]);
  }
  free(ImplGoal->extPred);
  
  LK_FILE_PUT1(fd,ImplGoal->findCodeFun);
  
  WriteHashTab(fd,&(ImplGoal->findCodeTab));
}

void WriteImplGoals(int fd)
{
  LK_VECTOR_Write(fd, &ImplGoals,&WriteImplGoal);
  LK_VECTOR_Free(&ImplGoals);
}

