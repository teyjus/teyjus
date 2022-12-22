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
#include "../tables/pervasives.h"
#include "module.h"
#include "vector.h"
#include "rename.h"
#include "file.h"
#include "tyskel.h"
#include "const.h"
#include "VectorRW.h"
#include "linker_message.h"

/*/////////////////////////////////////////////////////////////////////////////////////
//This file defines the code for using GConsts and LConsts/////
////////////////////////////////////////////////////////////////////////////////////*/

//////////////////////////////////////////////////////
//GConst Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
  Byte fixity;
  Byte precedence;
  Byte ty_env_size;
  char* name;
  TySkelInd ty_skel_index;
}Const_t;

int CheckGConstEqv(ConstInd i,Const_t newC);

ConstInd LoadGConst(int fd, struct Module_st* CMData)
{
  Const_t tmp;
  char* name;
  ConstInd index;

  tmp.fixity=LK_FILE_GET1(fd);
  tmp.precedence=LK_FILE_GET1(fd);
  tmp.ty_env_size=LK_FILE_GET1(fd);
  name=LK_FILE_GetString(fd);
  index=LK_RENAME_RenameConst(name);
  free(name);
  tmp.ty_skel_index=GetTySkelInd(fd,CMData);
  //if(!CheckGConstEqv(index,tmp))
  //  EM_THROW(LK_LinkError);
  return index;
}

void LoadGConsts(int fd, struct Module_st* CMData)
{
  int i;
  int count=CMData->GConstcount=LK_FILE_GET2(fd);
  mutter("Loading %d global constants\n",count);
  EM_TRY{
    CMData->GConst=(ConstInd*)EM_malloc(count*sizeof(ConstInd));
    for(i=0;i<count;i++)
      CMData->GConst[i]=LoadGConst(fd,CMData);
  }
  EM_CATCH{
    bad("Error while loading global constants\n");
    EM_RETHROW();
  }
}

Const_t* GConstTab=NULL;
size_t GConstTabSize=-1;
struct Vector LConsts;
size_t TotalLConsts=-1;
size_t TotalHConsts=-1;

TwoBytes PackConstInd(ConstInd ind){
  TwoBytes tmp;
  debug("Packing const [%d %d] to ",ind.gl_flag,ind.index);
  switch(ind.gl_flag){
    case PERVASIVE:
      tmp= ind.index;
      break;
    case GLOBAL:
      tmp= ind.index+PERV_CONST_NUM;
      break;
    case LOCAL:
      tmp= ind.index+PERV_CONST_NUM+GConstTabSize;
      break;
    case HIDDEN:
      tmp= -(ind.index+1);
      break;
    default:
      EM_THROW(LK_LinkError);
      break;
  }
  debug("%d\n",tmp);
  return tmp;
}

ConstInd UnPackConstInd(TwoBytes ind){
  ConstInd tmp;
  debug("[%d %d %d]\n",(int)PERV_CONST_NUM,(int)GConstTabSize,TotalLConsts);
  debug("UnPacking const %d to ",ind);
  if(ind<PERV_CONST_NUM){
    tmp.gl_flag=PERVASIVE;
    tmp.index=ind;
  } else if(ind<PERV_CONST_NUM+GConstTabSize){
    tmp.gl_flag=GLOBAL;
    tmp.index=ind-PERV_CONST_NUM;
  } else if(ind<PERV_CONST_NUM+GConstTabSize+TotalLConsts){
    tmp.gl_flag=LOCAL;
    tmp.index=ind-(PERV_CONST_NUM+GConstTabSize);
  } else {
    tmp.gl_flag=HIDDEN;
    tmp.index=-ind-1;
    if(tmp.index>=TotalHConsts){
      bad("ConstInd unpack failed\n");
      EM_THROW(LK_LinkError);
    }
  }
  debug("[%d %d]\n",tmp.gl_flag,tmp.index);
  return tmp;
}

void LoadTopGConst(int fd, struct Module_st* CMData, int i)
{
  GConstTab[i].fixity=LK_FILE_GET1(fd);
  GConstTab[i].precedence=LK_FILE_GET1(fd);
  GConstTab[i].ty_env_size=LK_FILE_GET1(fd);
  GConstTab[i].name=LK_FILE_GetString(fd);
  GConstTab[i].ty_skel_index=GetTySkelInd(fd,CMData);
  debug("GConst[%d]:%s\n",i,GConstTab[i].name);
}

void LoadTopGConsts(int fd, struct Module_st* CMData)
{
  int i;
  int count=GConstTabSize=CMData->GConstcount=LK_FILE_GET2(fd);
  detail("Loading %d top level global constants.\n",count);
  GConstTab=(Const_t*)EM_malloc(count*sizeof(Const_t));
  CMData->GConst=(ConstInd*)EM_malloc(count*sizeof(ConstInd));
  for(i=0;i<count;i++)
  {
    CMData->GConst[i].gl_flag=GLOBAL;
    CMData->GConst[i].index=i;
    LoadTopGConst(fd,CMData,i);
  }
}

void WriteGConst(int fd, int i)
{
  LK_FILE_PUT1(fd,GConstTab[i].fixity);
  LK_FILE_PUT1(fd,GConstTab[i].precedence);
  LK_FILE_PUT1(fd,GConstTab[i].ty_env_size);
  LK_FILE_PutString(fd,GConstTab[i].name);
  free(GConstTab[i].name);
  PutTySkelInd(fd,GConstTab[i].ty_skel_index);
}

void WriteGConsts(int fd)
{
  int i;
  int size=GConstTabSize;
  LK_FILE_PUT2(fd,size);
  for(i=0;i<size;i++)
  {
    WriteGConst(fd,i);
  }
  free(GConstTab);
}


//////////////////////////////////////////////////////
//LConst Load and Write Code
//////////////////////////////////////////////////////

void InitTLConsts()
{
  LK_VECTOR_Init(&LConsts,128,sizeof(Const_t));
}

void LoadLConst(int fd, struct Module_st* CMData,void* entry)
{
  Const_t* tmp=(Const_t*)entry;
  tmp->fixity=LK_FILE_GET1(fd);
  tmp->precedence=LK_FILE_GET1(fd);
  tmp->ty_env_size=LK_FILE_GET1(fd);
  tmp->ty_skel_index=GetTySkelInd(fd,(struct Module_st*)CMData);
}

void LoadLConsts(int fd, struct Module_st* CMData)
{
  LK_VECTOR_Read(fd,&LConsts,CMData,&(CMData->LConstAdj),LoadLConst);
}

void WriteLConst(int fd, void* entry)
{
  Const_t* tmp=(Const_t*)entry;
  LK_FILE_PUT1(fd,tmp->fixity);
  LK_FILE_PUT1(fd,tmp->precedence);
  LK_FILE_PUT1(fd,tmp->ty_env_size);
  PutTySkelInd(fd,tmp->ty_skel_index);
}

void WriteLConsts(int fd)
{
  TotalLConsts=LK_VECTOR_Size(&LConsts);
  LK_VECTOR_Write(fd, &LConsts,&WriteLConst);
  LK_VECTOR_Free(&LConsts);
}

//////////////////////////////////////////////////////
//HConst Load and Write Code
//////////////////////////////////////////////////////
struct Vector HConsts;

void InitTHConsts()
{
  LK_VECTOR_Init(&HConsts,128,sizeof(Const_t));
}

void LoadHConst(int fd, struct Module_st* CMData,void* entry)
{
  Const_t* tmp=(Const_t*)entry;
  
  tmp->ty_skel_index=GetTySkelInd(fd,CMData);
}

void LoadHConsts(int fd, struct Module_st* CMData)
{
  LK_VECTOR_Read(fd,&HConsts,CMData,&(CMData->HConstAdj),LoadHConst);
}

void WriteHConst(int fd, void* entry)
{
  Const_t* tmp=(Const_t*)entry;
  PutTySkelInd(fd,tmp->ty_skel_index);
}

void WriteHConsts(int fd)
{
  TotalHConsts=LK_VECTOR_Size(&HConsts);
  LK_VECTOR_Write(fd, &HConsts,&WriteHConst);
  LK_VECTOR_Free(&HConsts);
}

//////////////////////////////////////////////////////////////
void WriteConsts(int fd)
{
  debug("Writing Constant Tables at %lx\n",lseek(fd,0,SEEK_CUR));
  LK_FILE_PUT2(fd,LK_VECTOR_Size(&LConsts)+GConstTabSize+LK_VECTOR_Size(&HConsts));
  WriteGConsts(fd);
  WriteLConsts(fd);
  WriteHConsts(fd);
}

/////////////////////////////////////////////////////////////
//Utility Functions
////////////////////////////////////////////////////////////
int CheckGConstEqv(ConstInd i,Const_t newC)
{
  int b=1;
  Const_t* tmp;
  if(i.gl_flag==LOCAL)
    tmp=(Const_t*)LK_VECTOR_GetPtr(&LConsts,i.index);
  else
    tmp=&GConstTab[i.index];
  
  b=b&&tmp->fixity==newC.fixity;
  b=b&&tmp->precedence==newC.precedence;
  b=b&&tmp->ty_env_size==newC.ty_env_size;
  //b=b&&0==TySkelCmp(tmp->ty_skel_index,new.ty_skel_index);
  return b;
}
