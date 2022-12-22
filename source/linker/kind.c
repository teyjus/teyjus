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
#include "../include/standardlib.h"
#include "../system/error.h"
#include "datatypes.h"
#include "module.h"
#include "kind.h"
#include "vector.h"
#include "rename.h"
#include "file.h"
#include "VectorRW.h"
#include "linker_message.h"
#include "../tables/pervasives.h"
/*/////////////////////////////////////////////////////////////////////////////////////
//This file defines the code for using GKinds and LKinds/////
////////////////////////////////////////////////////////////////////////////////////*/

//////////////////////////////////////////////////////
//GKind Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
  Byte arity;
  char* name;
}Kind_t;

KindInd LoadGKind(int fd)
{
  Byte arity=LK_FILE_GET1(fd);
  char* name=LK_FILE_GetString(fd);
  KindInd index=LK_RENAME_RenameKind(name);
  Byte oldarity=CheckKindArity(index);
  if(arity!=oldarity)
  {
    printf("Kind Arity Mismatch: Kind %s should have arity %d, has arity %d.\n",name,oldarity,arity);
    free(name);
    EM_THROW(LK_LinkError);
  }
  free(name);
  return index;
}

void LoadGKinds(int fd, struct Module_st* CMData)
{
  size_t i;
  
  TwoBytes count=CMData->GKindcount=LK_FILE_GET2(fd);
  mutter("Loading %d global kinds\n",count);
  EM_TRY{
    CMData->GKind=(KindInd *)EM_malloc(count*sizeof(KindInd));
    for(i=0;i<count;i++)
    {
      CMData->GKind[i]=LoadGKind(fd);
    }
  }
  EM_CATCH{
    bad("Error while loading global kinds\n");
    EM_RETHROW();
  }
}

Kind_t* GKindTab=NULL;
size_t GKindTabSize=-1;
struct Vector LKinds;
size_t TotalLKinds=-1;

TwoBytes PackKindInd(KindInd ind){
  TwoBytes tmp;
  switch(ind.gl_flag){
    case PERVASIVE:
      tmp=ind.index;
      break;
    case GLOBAL:
      tmp=ind.index+PERV_KIND_NUM;
      break;
    case LOCAL:
      tmp=ind.index+PERV_KIND_NUM+GKindTabSize;
      break;
    default:
      EM_THROW(LK_LinkError);
      break;
  }
  return tmp;
}

KindInd UnPackKindInd(TwoBytes ind){
  KindInd tmp;
  if(ind<PERV_KIND_NUM){
    tmp.gl_flag=PERVASIVE;
    tmp.index=ind;
  } else if(ind<PERV_KIND_NUM+GKindTabSize){
    tmp.gl_flag=GLOBAL;
    tmp.index=ind-PERV_KIND_NUM;
  } else if(ind<PERV_KIND_NUM+GKindTabSize+TotalLKinds){
    tmp.gl_flag=LOCAL;
    tmp.index=ind-(PERV_KIND_NUM+GKindTabSize);
  }else {
    bad("KindInd unpack of %d failed: [%d:%d:%ld]\n",\
        ind,PERV_KIND_NUM,GKindTabSize,LK_VECTOR_Size(&LKinds));
    EM_THROW(LK_LinkError);
  }
  return tmp;
}

KindInd LoadTopGKind(int fd,size_t i)
{
  KindInd tmp={GLOBAL,i};
  tmp.index=i;
  tmp.gl_flag=GLOBAL;
  
  GKindTab[i].arity=LK_FILE_GET1(fd);
  GKindTab[i].name=LK_FILE_GetString(fd);
  //printf("RGK(%d,%s)%x:\n",GKindTab[i].arity,GKindTab[i].name,GKindTab[i].name);fflush(stdout);
  return tmp;
}

void LoadTopGKinds(int fd, struct Module_st* CMData)
{
  size_t i;
  TwoBytes count=GKindTabSize=CMData->GKindcount=LK_FILE_GET2(fd);
  detail("Loading %d top level global kinds.\n",count);
  GKindTab=(Kind_t *)EM_malloc(GKindTabSize*sizeof(Kind_t));
  
  CMData->GKind=(KindInd *)EM_malloc(count*sizeof(MarkInd));
  
  for(i=0;i<count;i++)
  {
    CMData->GKind[i]=LoadTopGKind(fd,i);
  }
}

void WriteGKind(int fd, size_t i)
{
  //printf("GK(%d,%s)\n",GKindTab[i].arity,GKindTab[i].name);fflush(stdout);
  LK_FILE_PUT1(fd,GKindTab[i].arity);
  LK_FILE_PutString(fd,GKindTab[i].name);
  free(GKindTab[i].name);
}

void WriteGKinds(int fd)
{
  size_t i;
  TwoBytes tmp=GKindTabSize;
  LK_FILE_PUT2(fd,tmp);
  for(i=0;i<tmp;i++)
  {
    WriteGKind(fd,i);
  }
  free(GKindTab);
  GKindTab=NULL;
}


//////////////////////////////////////////////////////
//LKind Load and Write Code
//////////////////////////////////////////////////////

void InitTLKinds()
{
  LK_VECTOR_Init(&LKinds,128,sizeof(Kind_t));
}

void LoadLKind(int fd, struct Module_st* CMData,void* entry)
{
  Kind_t* tmp=(Kind_t*)entry;
  tmp->arity=LK_FILE_GET1(fd);
}

void LoadLKinds(int fd, struct Module_st* CMData)
{
  LK_VECTOR_Read(fd,&LKinds,CMData,&(CMData->LKindAdj),LoadLKind);
}

void WriteLKind(int fd, void* ent)
{
  LK_FILE_PUT1(fd,((Kind_t*)ent)->arity);
}

void WriteLKinds(int fd)
{
  TotalLKinds=LK_VECTOR_Size(&LKinds);
  LK_VECTOR_Write(fd, &LKinds,&WriteLKind);
  LK_VECTOR_Free(&LKinds);
}

////////////////////////////////////////////////
void WriteKinds(int fd)
{
  debug("Writing Kind Tables at %lx\n",lseek(fd,0,SEEK_CUR));
  LK_FILE_PUT2(fd,LK_VECTOR_Size(&LKinds)+GKindTabSize);
  WriteGKinds(fd);
  WriteLKinds(fd);
}

/////////////////////////////////////////////////////////////
//Utility Functions
////////////////////////////////////////////////////////////
Byte CheckKindArity(KindInd i)
{
  if(i.gl_flag==LOCAL)
  {
    return ((Kind_t*)LK_VECTOR_GetPtr(&LKinds,i.index))->arity;
  }
  //GLOBAL_KIND
  return GKindTab[i.index].arity;
}
