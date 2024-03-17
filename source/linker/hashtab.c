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
#include <sys/types.h>
#include "../include/standardlib.h"
#include "vector.h"
#include "datatypes.h"
#include "module.h"
#include "file.h"
#include "hashtab.h"
#include "code.h"
#include "VectorRW.h"
#include "linker_message.h"
#include "importtab.h"
//////////////////////////////////////////////////////
//HashTab Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
  ConstInd index;
  CodeInd addr;
}HashTabEnt;

struct Vector HashTabs;

void HASH_AddEntry(HashTab_t* HashTab, HashTabEnt* entry);

void InitTHashTabs()
{
  LK_VECTOR_Init(&HashTabs,32,sizeof(HashTab_t));
}

void LoadHashTabEnt(int fd, struct Module_st* CMData,void* entry)
{
  HashTabEnt* tmp = (HashTabEnt*)entry;
  tmp->index=GetConstInd(fd,CMData);
  tmp->addr=GetCodeInd(fd,CMData);
  debug("HashTabEnt[%d,%d]=%lx\n",tmp->index.gl_flag,tmp->index.index,tmp->addr);
}

void LoadHashTab(int fd, struct Module_st* CMData,void* entry)
{
  Adjust_t ignore_me;
  
  debug("Loading HashTab at %lx\n",lseek(fd,0,SEEK_CUR));
  
  LK_VECTOR_Init((struct Vector*)entry,0,sizeof(HashTabEnt));
  LK_VECTOR_Read(fd,(struct Vector*)entry,CMData,&ignore_me,LoadHashTabEnt);
}

void LoadHashTabs(int fd, struct Module_st* CMData)
{
  LK_VECTOR_Read(fd,&HashTabs,CMData,&(CMData->HashTabAdj),LoadHashTab);
}

void WriteHashTabEnt(int fd,void* entry)
{
  HashTabEnt* tmp = (HashTabEnt*)entry;
  PutConstInd(fd,tmp->index);
  PutCodeInd(fd,tmp->addr);
  debug("Writing HashTabEnt[%d,%d]=%lx\n",tmp->index.gl_flag,tmp->index.index,tmp->addr);
}

void WriteHashTab(int fd, void* entry)
{
  debug("Writing HashTab at %lx\n",lseek(fd,0,SEEK_CUR));
  LK_VECTOR_Write(fd,(struct Vector*)entry,&WriteHashTabEnt);
  if(LK_VECTOR_Size((struct Vector*)entry)!=0)
    LK_VECTOR_Free((struct Vector*)entry);
}

void WriteHashTabs(int fd)
{
  LK_VECTOR_Write(fd, &HashTabs,&WriteHashTab);
  LK_VECTOR_Free(&HashTabs);
}

int HashTabSearch(HashTab_t* HashTab, ConstInd x)
{
  HashTabEnt* tmp=(HashTabEnt *)LK_VECTOR_GetPtr((struct Vector*)HashTab,0);
  int i;
  int size=LK_VECTOR_Size((struct Vector*)HashTab);
  for(i=0;i<size;i++)
  {
    if((tmp[i].index.gl_flag==x.gl_flag)&&(tmp[i].index.index==x.index))
      return i;
  }
  return -1;
}

Byte MergeHashTabs(HashTabInd a, HashTabInd b,Byte n)
{
  HashTab_t* pa;
  HashTab_t* pb;
  HashTabEnt* tmpa;
  HashTabEnt* tmpb;
  int size;
  int i,j;
  

  debug("Merging HashTabs %d and %d.\n",a,b);
  pa=(HashTab_t*)LK_VECTOR_GetPtr(&HashTabs,a);
  pb=(HashTab_t*)LK_VECTOR_GetPtr(&HashTabs,b);
  
  size=LK_VECTOR_Size((struct Vector*)pb);
  tmpa=(HashTabEnt*)LK_VECTOR_GetPtr((struct Vector*)pa,0);
  tmpb=(HashTabEnt*)LK_VECTOR_GetPtr((struct Vector*)pb,0);
  for(i=0;i<size;i++)
  {
    j=HashTabSearch(pa,tmpb[i].index);
    if(j!=-1)
    {
      tmpa[j].addr=MergeSubSequence(tmpa[j].addr,tmpb[i].addr,n);
    }
    else
    {
      HASH_AddEntry(pa,tmpb+i);
      tmpa=(HashTabEnt*)LK_VECTOR_GetPtr((struct Vector*)pa,0);
    }
  }
  LK_VECTOR_Free((struct Vector*)pb);
  return LK_VECTOR_Size((struct Vector*)pa);
}

void MergeFindCodeTabs(HashTab_t* pa, HashTab_t* pb)
{
  int size=LK_VECTOR_Size((struct Vector*)pb);
  int i,j;
  HashTabEnt* tmpa=(HashTabEnt*)LK_VECTOR_GetPtr((struct Vector*)pa,0);
  HashTabEnt* tmpb=(HashTabEnt*)LK_VECTOR_GetPtr((struct Vector*)pb,0);
  for(i=0;i<size;i++)
  {
    j=HashTabSearch(pa,tmpb[i].index);
    if(j!=-1)
    {
      tmpa[j].addr=MergeDefs(tmpa[j].addr,tmpb[i].addr);
    }
    else
    {
      if(shouldTidySwitchOnReg(tmpb[i].index))
      {
        debug("Need to tidy [%d:%d]\n",
              tmpb[i].index.gl_flag,
              tmpb[i].index.index);
        TidySwitchOnReg(&(tmpb[i].addr));
      }
      HASH_AddEntry(pa,tmpb+i);
      tmpa=(HashTabEnt*)LK_VECTOR_GetPtr((struct Vector*)pa,0);
    }
  }
  LK_VECTOR_Free((struct Vector*)pb);
}

void HASH_AddEntry(HashTab_t* HashTab, HashTabEnt* entry)
{
  HashTabEnt* newT=(HashTabEnt *)LK_VECTOR_GetPtr((struct Vector*)HashTab,LK_VECTOR_Grow((struct Vector*)HashTab,1));
  newT->index=entry->index;
  newT->addr=entry->addr;
}

CodeInd HashCodeAddr(HashTab_t* HashTab, ConstInd x)
{
  HashTabEnt* tmp=(HashTabEnt *)LK_VECTOR_GetPtr((struct Vector*)HashTab,0);
  int i;
  int size=LK_VECTOR_Size((struct Vector*)HashTab);
  for(i=0;i<size;i++)
  {
    if((tmp[i].index.gl_flag==x.gl_flag)&&(tmp[i].index.index==x.index))
      return tmp[i].addr;
  }
  return -1;
}
