#include <stdio.h>
#include <stdlib.h>
#include "datatypes.h"
#include "module.h"
#include "kind.h"
#include "vector.h"
#include "rename.h"
#include "file.h"
#include "../system/error.h"
/*/////////////////////////////////////////////////////////////////////////////////////
//This file defines the code for using GKinds and LKinds/////
////////////////////////////////////////////////////////////////////////////////////*/

//////////////////////////////////////////////////////
//GKind Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
  int arity;
  Name name;
}TGKind_t;

//struct Vector GKinds;

KindInd LoadTopGKind(int fd, int i);
KindInd LoadGKind();
void WriteGKind(int fd, int i);

void WriteGKinds();
void WriteLKinds();

void LoadGKinds()
{
  int i;
  
  TwoBytes count=CM->GKindcount=GET2();
  CM->GKind=EM_malloc(count*sizeof(KindInd));
  printf("Loading %d Global Kinds.\n",count);//DEBUG
  for(i=0;i<count;i++)
  {
    CM->GKind[i]=LoadGKind();
  }
}

KindInd LoadGKind()
{
  int arity=GET1();
  int oldarity;
  Name name;
  GetName(&name);
  KindInd index=LK_RENAME_RenameKind(name);
  oldarity=CheckKindArity(index);
  if(arity!=oldarity)
  {
    printf("Kind Arity Mismatch: Kind %s should have arity %d, has arity %d.\n",name.string,oldarity,arity);
    exit(0);
  }
  Clear(name);
  return index;
}

TGKind_t* GKindTab=NULL;
int GKindTabSize=-1;

void LK_KIND_FreeGKinds()
{
  free(GKindTab);
}

void LoadTopGKinds(int fd, struct Module_st* CMData)
{
  int i;
  TwoBytes count=GKindTabSize=CMData->GKindcount=LK_FILE_GET2(fd);
  
  GKindTab=EM_malloc(GKindTabSize*sizeof(TGKind_t));
  
  CMData->GKind=EM_malloc(count);
  
  for(i=0;i<count;i++)
  {
    CMData->GKind[i]=LoadTopGKind(fd,i);
  }
}

KindInd LoadTopGKind(int fd,int i)
{
  KindInd tmp={GLOBAL,i};
  tmp.index=i;
  tmp.gl_flag=GLOBAL;
  
  GKindTab[i].arity=LK_FILE_GET1(fd);
  LK_FILE_GetName(fd,&(GKindTab[i].name));
  
  return tmp;
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
}

void WriteGKind(int fd, int i)
{
  LK_FILE_PUT1(fd,GKindTab[i].arity);
  LK_FILE_PutName(fd,GKindTab[i].name);
}


//////////////////////////////////////////////////////
//LKind Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
  int arity;
}TLKind_t;

struct Vector LKinds;

void LoadLKind(int fd, int i);
void WriteLKind(int fd, int i);

void InitTLKinds()
{
  InitVec(&LKinds,128,sizeof(TLKind_t));
}

void LoadLKinds(int fd, struct Module_st* CMData)
{
  int i;
  TwoBytes count=LK_FILE_GET2(fd);
  CMData->LKindcount=count;
  int offset=CMData->LKindoffset=Extend(&LKinds,count);
  //printf("Loading %d Local Kinds.\n",count);//DEBUG
  
  for(i=0;i<count;i++)
  {
    LoadLKind(fd,offset+i);
  }
}

void LoadLKind(int fd, int i)
{
  TLKind_t* tmp=Fetch(&LKinds,i);
  tmp->arity=LK_FILE_GET1(fd);
}

void WriteLKinds(int fd)
{
  int i;
  TwoBytes tmp=LKinds.numEntries;
  LK_FILE_PUT2(fd,tmp);
  for(i=0;i<tmp;i++)
  {
    WriteLKind(fd,i);
  }
}

void WriteLKind(int fd, int i)
{
  TLKind_t* tmp=Fetch(&LKinds,i);
  LK_FILE_PUT1(fd,tmp->arity);
}

////////////////////////////////////////////////
void WriteKinds()
{
  PUT2(LKinds.numEntries+GKindTabSize);
  WriteGKinds(PeekOutput());
  WriteLKinds(PeekOutput());
}

/////////////////////////////////////////////////////////////
//Utility Functions
////////////////////////////////////////////////////////////
int CheckKindArity(KindInd i)
{
  if(i.gl_flag==LOCAL)
  {
    return ((TLKind_t*)Fetch(&LKinds,i.index))->arity;
  }
  //GLOBAL_KIND
  return GKindTab[i.index].arity;
}
