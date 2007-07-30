#include <stdio.h>
#include <unistd.h>
#include "../system/error.h"
#include "../tables/pervasives.h"
#include "module.h"
#include "vector.h"
#include "rename.h"
#include "file.h"
#include "tyskel.h"
#include "const.h"
#include "VectorRW.h"
#include "message.h"

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

int CheckGConstEqv(ConstInd i,Const_t new);

ConstInd LoadGConst(int fd, struct Module_st* CMData)
{
  Const_t tmp;
  tmp.fixity=LK_FILE_GET1(fd);
  tmp.precedence=LK_FILE_GET1(fd);
  tmp.ty_env_size=LK_FILE_GET1(fd);
  char* name=LK_FILE_GetString(fd);
  ConstInd index=LK_RENAME_RenameConst(name);
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
  CMData->GConst=(ConstInd*)EM_malloc(count*sizeof(ConstInd));
  for(i=0;i<count;i++)
    CMData->GConst[i]=LoadGConst(fd,CMData);
}

Const_t* GConstTab=NULL;
int GConstTabSize=-1;
struct Vector LConsts;

TwoBytes PackConstInd(ConstInd ind){
  switch(ind.gl_flag){
    case PERVASIVE:
      return ind.index;
    case GLOBAL:
      return ind.index+PERV_CONST_NUM;
    case LOCAL:
      return ind.index+PERV_CONST_NUM+GConstTabSize;
    case HIDDEN:
      return -(ind.index+1);
  }
  EM_THROW(LK_LinkError);
}

ConstInd UnPackConstInd(TwoBytes ind){
  ConstInd tmp;
  if(ind<PERV_CONST_NUM){
    tmp.gl_flag=PERVASIVE;
    tmp.index=ind;
  } else if(PERV_CONST_NUM<=ind && ind<PERV_CONST_NUM+GConstTabSize){
    tmp.gl_flag=GLOBAL;
    tmp.index=ind-PERV_CONST_NUM;
  } else if(PERV_CONST_NUM+GConstTabSize<=ind &&\
            ind<PERV_CONST_NUM+GConstTabSize+LK_VECTOR_Size(&LConsts)){
    tmp.gl_flag=LOCAL;
    tmp.index=ind-(PERV_CONST_NUM+GConstTabSize);
  } else if(((TwoBytes)-LK_VECTOR_Size(&LConsts))<=ind){
    tmp.gl_flag=HIDDEN;
    tmp.index=-ind-1;
  } else {
    bad("ConstInd unpack failed");
    EM_THROW(LK_LinkError);
  }
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
  LK_VECTOR_Write(fd, &HConsts,&WriteHConst);
  LK_VECTOR_Free(&HConsts);
}

//////////////////////////////////////////////////////////////
void WriteConsts(int fd)
{
  debug("Writing Constant Tables at %x\n",lseek(fd,0,SEEK_CUR));
  LK_FILE_PUT2(fd,LK_VECTOR_Size(&LConsts)+GConstTabSize+LK_VECTOR_Size(&HConsts));
  WriteGConsts(fd);
  WriteLConsts(fd);
  WriteHConsts(fd);
}

/////////////////////////////////////////////////////////////
//Utility Functions
////////////////////////////////////////////////////////////
int CheckGConstEqv(ConstInd i,Const_t new)
{
  int b=1;
  Const_t* tmp;
  if(i.gl_flag==LOCAL)
    tmp=(Const_t*)LK_VECTOR_GetPtr(&LConsts,i.index);
  else
    tmp=&GConstTab[i.index];
  
  b=b&&tmp->fixity==new.fixity;
  b=b&&tmp->precedence==new.precedence;
  b=b&&tmp->ty_env_size==new.ty_env_size;
  //b=b&&0==TySkelCmp(tmp->ty_skel_index,new.ty_skel_index);
  return b;
}
