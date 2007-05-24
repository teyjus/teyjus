#include <stdlib.h>
#include <stdio.h>
#include <obstack.h>
#include "module.h"
#include "tyskel.h"
#include "kind.h"
#include "vector.h"
#include "file.h"
#include "VectorRW.h"

#define obstack_chunk_alloc EM_malloc
#define obstack_chunk_free free

//////////////////////////////////////////////////////
//TySkel Load and Write Code
//////////////////////////////////////////////////////

typedef struct{
  void* TySkel;
  int size;
}TySkel_t;

struct Vector TySkels;

struct obstack TySkelStack;
void *StackBot=NULL;

void LK_TYSKEL_obstack_alloc_failed_handler(void)
{
  exit(0);
}

void InitTTySkels()
{
  LK_VECTOR_Init(&TySkels,128,sizeof(TySkel_t));
  obstack_alloc_failed_handler=&LK_TYSKEL_obstack_alloc_failed_handler;
  obstack_init(&TySkelStack);
}

void LoadTySkel(int fd, struct Module_st* CMData)
{
  int arity,j;
  KindInd KTMP;
  Byte type=LK_FILE_GET1(fd);
  obstack_1grow(&TySkelStack,type);
  switch(type)
  {
    case ARROW:
      LoadTySkel(fd,CMData);
      LoadTySkel(fd,CMData);
      break;
      
    case KIND:
      KTMP=GetKindInd(fd,CMData);
      obstack_1grow(&TySkelStack,KTMP.gl_flag);
      obstack_grow(&TySkelStack,&(KTMP.index),sizeof(TwoBytes));
      arity=CheckKindArity(KTMP);
      for(j=0;j<arity;j++)
        LoadTySkel(fd,CMData);
      break;
      
    case VARIABLE:
    default:
      obstack_1grow(&TySkelStack,LK_FILE_GET1(fd));
      break;
  }
}

void LoadTySkels(int fd, struct Module_st* CMData)
{
  if(StackBot==NULL)
    StackBot=obstack_alloc(&TySkelStack,1);
  int i;
  int count=CMData->TySkelAdj.count=LK_FILE_GET2(fd);
  int offset=LK_VECTOR_Grow(&TySkels,count);
  CMData->TySkelAdj.offset=offset;
  TySkel_t* tab=LK_VECTOR_GetPtr(&TySkels,offset);
  for(i=0;i<count;i++)
  {
    LoadTySkel(fd,CMData);
    tab[i].size=obstack_object_size(&TySkelStack);
    tab[i].TySkel=obstack_finish(&TySkelStack);
  }
}

void WriteTySkel(int fd, void* entry)
{
  TySkel_t* p=entry;
  LK_FILE_PUTN(fd,p->TySkel,p->size);
}

void WriteTySkels(int fd)
{
  LK_VECTOR_Write(fd,&TySkels,WriteTySkel);
  LK_VECTOR_Free(&TySkels);
  obstack_free(&TySkelStack,StackBot);
  StackBot=NULL;
}

int TySkelCmp(TySkelInd a, TySkelInd b)
{
  if(a==b)
    return 0;
  
  ///\todo Actually make sure type skeletons match.
  return 0;
}
