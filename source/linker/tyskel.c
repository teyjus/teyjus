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
#include <stdlib.h>
#include <stdio.h>
#include "../include/standardlib.h"
#include "module.h"
#include "tyskel.h"
#include "kind.h"
#include "vector.h"
#include "file.h"
#include "VectorRW.h"
#include "linker_message.h"

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
#if BYTE_ORDER == LITTLE_ENDIAN
      KTMP.index = bswap_16(KTMP.index);
#elif BYTE_ORDER == BIG_ENDIAN
#else
#error Unknown BYTE_ORDER
#endif
      obstack_grow(&TySkelStack,&(KTMP.index),sizeof(TwoBytes));
      arity=LK_FILE_GET1(fd);
      obstack_1grow(&TySkelStack,arity);
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
  int i;
  int offset;
  int count;
  TySkel_t* tab;
  
  if(StackBot==NULL)
    StackBot=obstack_alloc(&TySkelStack,1);
  
  count=CMData->TySkelAdj.count=LK_FILE_GET2(fd);
  offset=LK_VECTOR_Grow(&TySkels,count);
  CMData->TySkelAdj.offset=offset;
  tab=(TySkel_t *)LK_VECTOR_GetPtr(&TySkels,offset);
  for(i=0;i<count;i++)
  {
    LoadTySkel(fd,CMData);
    tab[i].size=obstack_object_size(&TySkelStack);
    tab[i].TySkel=obstack_finish(&TySkelStack);
  }
}

void WriteTySkel(int fd, void* entry)
{
  TySkel_t* p=(TySkel_t *)entry;
  LK_FILE_PUTN(fd,p->TySkel,p->size);
}

void WriteTySkels(int fd)
{
  debug("Writing Type Skels at %lx\n",lseek(fd,0,SEEK_CUR));
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
