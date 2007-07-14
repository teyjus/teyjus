#include <stdio.h>
#include "../system/error.h"
#include "datatypes.h"
#include "module.h"
#include "kind.h"
#include "vector.h"
#include "rename.h"
#include "file.h"
#include "VectorRW.h"
#include "message.h"
/*/////////////////////////////////////////////////////////////////////////////////////
//This file defines the code for using GKinds and LKinds/////
////////////////////////////////////////////////////////////////////////////////////*/

//////////////////////////////////////////////////////
//GKind Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
  int arity;
  char* name;
}Kind_t;

KindInd LoadGKind(int fd)
{
  int arity=LK_FILE_GET1(fd);
  int oldarity;
  char* name=LK_FILE_GetString(fd);
  KindInd index=LK_RENAME_RenameKind(name);
  oldarity=CheckKindArity(index);
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
  int i;
  
  TwoBytes count=CMData->GKindcount=LK_FILE_GET2(fd);
  CMData->GKind=EM_malloc(count*sizeof(KindInd));
  for(i=0;i<count;i++)
  {
    CMData->GKind[i]=LoadGKind(fd);
  }
}

Kind_t* GKindTab=NULL;
int GKindTabSize=-1;

KindInd LoadTopGKind(int fd,int i)
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
  int i;
  TwoBytes count=GKindTabSize=CMData->GKindcount=LK_FILE_GET2(fd);
  detail("Loading %d top level global kinds.\n",count);
  GKindTab=EM_malloc(GKindTabSize*sizeof(Kind_t));
  
  CMData->GKind=EM_malloc(count*sizeof(MarkInd));
  
  for(i=0;i<count;i++)
  {
    CMData->GKind[i]=LoadTopGKind(fd,i);
  }
}

void WriteGKind(int fd, int i)
{
  //printf("GK(%d,%s)\n",GKindTab[i].arity,GKindTab[i].name);fflush(stdout);
  LK_FILE_PUT1(fd,GKindTab[i].arity);
  LK_FILE_PutString(fd,GKindTab[i].name);
  free(GKindTab[i].name);
}

void WriteGKinds(int fd)
{
  int i;
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
struct Vector LKinds;

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
  LK_VECTOR_Write(fd, &LKinds,&WriteLKind);
  LK_VECTOR_Free(&LKinds);
}

////////////////////////////////////////////////
void WriteKinds(int fd)
{
  LK_FILE_PUT2(fd,LK_VECTOR_Size(&LKinds)+GKindTabSize);
  WriteGKinds(fd);
  WriteLKinds(fd);
}

/////////////////////////////////////////////////////////////
//Utility Functions
////////////////////////////////////////////////////////////
int CheckKindArity(KindInd i)
{
  if(i.gl_flag==LOCAL)
  {
    return ((Kind_t*)LK_VECTOR_GetPtr(&LKinds,i.index))->arity;
  }
  //GLOBAL_KIND
  return GKindTab[i.index].arity;
}
