#include <stdlib.h>
#include <stdio.h>
#include "module.h"
#include "tyskel.h"
#include "kind.h"
#include "vector.h"
#include "file.h"

//////////////////////////////////////////////////////
//TySkel Load and Write Code
//////////////////////////////////////////////////////

#define ARROW 0
#define PKIND 1
#define LKIND 2
#define GKIND 3
#define VARIABLE 4

struct Vector TySkels;

int LoadTySkel(struct Vector* TySkel,int i);
void WriteTySkel(int i);

void InitTTySkels()
{
  InitVec(&TySkels,128,sizeof(struct Vector));
}

int LoadTySkel(struct Vector* TySkel,int i)
{
  int arity,j;
  KindInd KTMP;
  Byte type;
  Byte* tyStr=(Byte*)Fetch(TySkel,i++);
  tyStr[0]=type=GET1();
  switch(type)
  {
    case ARROW:
      Extend(TySkel,3);
      i=LoadTySkel(TySkel,i);
      i=LoadTySkel(TySkel,i);
      break;
      
    case PKIND:
    case LKIND:
      if(type==PKIND)
        type=PERVASIVE;
      else
        type=LOCAL;
      KTMP=FindKindInd(type,GET2());
      arity=CheckKindArity(KTMP);
      Extend(TySkel,1+arity*2);
      tyStr=(Byte*)Fetch(TySkel,i);
      *(TwoBytes*)tyStr=KTMP.index;
      i+=2;
      for(j=0;j<arity;j++)
        i=LoadTySkel(TySkel,i);
      break;
      
    case GKIND:
      type=GLOBAL;
      KTMP=FindKindInd(type,GET2());
      arity=CheckKindArity(KTMP);
      Extend(TySkel,1+arity*2);
      tyStr=(Byte*)Fetch(TySkel,i);
      *(TwoBytes*)tyStr=KTMP.index;
      if(KTMP.gl_flag==GLOBAL)
        *(tyStr-1)=GKIND;
      else
        *(tyStr-1)=LKIND;
      i+=2;
      for(j=0;j<arity;j++)
        i=LoadTySkel(TySkel,i);
      break;
      
    case VARIABLE:
    default:
      tyStr[1]=GET1();
      i++;
      break;
  }
  return i;
}

void LoadTySkels()
{
  int i;
  struct Vector* tmp;
  TwoBytes count=CM->TySkelcount=GET2();
  int offset=CM->TySkeloffset=Extend(&TySkels,(int)count);
  //CM->TySkel=AllocateLTySkels(count);
  tmp=(struct Vector*)Fetch(&TySkels,offset);
  for(i=0;i<count;i++)
  {
    InitVec(tmp+i,32,1);
    Extend(tmp+i,2);
    LoadTySkel(tmp+i,0);
  }
}

void WriteTySkels()
{
  int i;
  PUT2(TySkels.numEntries);
  for(i=0;i<TySkels.numEntries;i++)
  {
    WriteTySkel(i);
  }
}

void WriteTySkel(int i)
{
  struct Vector* tmp=(struct Vector*)Fetch(&TySkels,i);
  PUTN(tmp->entry,tmp->numEntries);
}

int TySkelCmp(TySkelInd a, TySkelInd b)
{
  if(a==b)
    return 0;
  
  struct Vector* ap=Fetch(&TySkels,a);
  struct Vector* bp=Fetch(&TySkels,b);
  
  int size=ap->numEntries;
  if(size!=bp->numEntries)
    return -1;
  
  char* tmpa=Fetch(ap,0);
  char* tmpb=Fetch(bp,0);
  int i;
  for(i=0;i<size;i++)
  {
    if(tmpa[i]!=tmpb[i])
      return -1;
  }
  
  return 0;
}
