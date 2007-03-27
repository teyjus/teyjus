#include <stdio.h>
#include "system/error.h"
#include "module.h"
#include "vector.h"
#include "rename.h"
#include "file.h"
#include "tyskel.h"
#include "const.h"
#include "linker/VectorRW.h"

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
  Byte neededness;
  char* name;
  TySkelInd ty_skel_index;
}Const_t;

int CheckGConstEqv(ConstInd i,Const_t new);

ConstInd LoadGConst()
{
  Const_t tmp;
  tmp.fixity=GET1();
  tmp.precedence=GET1();
  tmp.ty_env_size=GET1();;
  //tmp.ty_preserving_info=GET1();
  Name name;
  GetName(&name);
  ConstInd index=LK_RENAME_RenameConst(name);
  tmp.ty_skel_index=GetTySkelInd(PeekInput(),CM);
  if(!CheckGConstEqv(index,tmp))
  {
    EM_THROW(LK_LinkError);
  }
  return index;
}

void LoadGConsts()
{
  int i;
  int count=CM->GConstcount=GET2();
  CM->GConst=(ConstInd*)EM_malloc(count*sizeof(ConstInd));
  printf("Loading %d Global Constants.\n",count);//DEBUG
  for(i=0;i<count;i++)
  {
    CM->GConst[i]=LoadGConst();
  }
}

Const_t* GConstTab=NULL;
int GConstTabSize=-1;

void LoadTopGConst(int fd, struct Module_st* CMData, int i)
{
  GConstTab[i].fixity=LK_FILE_GET1(fd);
  GConstTab[i].precedence=LK_FILE_GET1(fd);
  GConstTab[i].ty_env_size=LK_FILE_GET1(fd);;
  GConstTab[i].neededness=LK_FILE_GET1(fd);
  GConstTab[i].name=LK_FILE_GetString(fd);
  GConstTab[i].ty_skel_index=GetTySkelInd(fd,CMData);
}

void LoadTopGConsts(int fd, struct Module_st* CMData)
{
  int i;
  int count=GConstTabSize=LK_FILE_GET2(fd);
  GConstTab=(Const_t*)EM_malloc(count*sizeof(Const_t));
  CMData->GConst=(ConstInd*)EM_malloc(count*sizeof(ConstInd));
  for(i=0;i<count;i++)
  {
    CMData->GConst[i].gl_flag=GLOBAL;
    CMData->GConst[i].gl_flag=i;
    LoadTopGConst(fd,CMData,i);
  }
}

void WriteGConst(int fd, int i)
{
  LK_FILE_PUT1(fd,GConstTab[i].fixity);
  LK_FILE_PUT1(fd,GConstTab[i].precedence);
  LK_FILE_PUT1(fd,GConstTab[i].ty_env_size);
  LK_FILE_PUT1(fd,GConstTab[i].neededness);
  LK_FILE_PutString(fd,GConstTab[i].name);
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
}


//////////////////////////////////////////////////////
//LConst Load and Write Code
//////////////////////////////////////////////////////
struct Vector LConsts;

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
  tmp->neededness=LK_FILE_GET1(fd);
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
  LK_FILE_PUT1(fd,tmp->neededness);
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
  
  tmp->ty_env_size=LK_FILE_GET1(fd);
  tmp->neededness=LK_FILE_GET1(fd);
  tmp->ty_skel_index=GetTySkelInd(fd,CMData);
}

void LoadHConsts(int fd, struct Module_st* CMData)
{
  LK_VECTOR_Read(fd,&HConsts,CMData,&(CMData->HConstAdj),LoadHConst);
}

void WriteHConst(int fd, void* entry)
{
  Const_t* tmp=(Const_t*)entry;
  LK_FILE_PUT1(fd,tmp->ty_env_size);
  LK_FILE_PUT1(fd,tmp->neededness);
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
  b=b&&0==TySkelCmp(tmp->ty_skel_index,new.ty_skel_index);
  return b;
}
