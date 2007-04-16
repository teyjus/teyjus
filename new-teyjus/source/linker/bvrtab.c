#include "vector.h"
#include "file.h"
#include "datatypes.h"
#include "module.h"
#include "code.h"

#include "VectorRW.h"
//////////////////////////////////////////////////////
//BvrTab Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
  Byte index;
  CodeInd addr;
}BvrTabEnt;

typedef struct Vector BvrTab_t;

struct Vector BvrTabs;

void BVR_AddEntry(struct Vector* BvrTab, BvrTabEnt* entry);

void InitTBvrTabs()
{
  LK_VECTOR_Init(&BvrTabs,32,sizeof(BvrTab_t));
}

void LoadBvrTabEnt(int fd, struct Module_st* CMData,void* entry)
{
  BvrTabEnt* tmp=(BvrTabEnt*)entry;
  tmp->index=LK_FILE_GET1(fd);
  tmp->addr=GetCodeInd(fd,CMData);
}

void LoadBvrTab(int fd, struct Module_st* CMData,void* entry)
{
  Adjust_t ignore_me;
  LK_VECTOR_Init((struct Vector*)entry,0,sizeof(BvrTabEnt));
  LK_VECTOR_Read(fd,(struct Vector*)entry,CMData,&ignore_me,LoadBvrTabEnt);
}

void LoadBvrTabs(int fd, struct Module_st* CMData)
{
  LK_VECTOR_Read(fd,&BvrTabs,CMData,&(CMData->BvrTabAdj),LoadBvrTab);
}

void WriteBvrTabEnt(int fd,void* entry)
{
  BvrTabEnt* tmp = (BvrTabEnt*)entry;
  LK_FILE_PUT1(fd,tmp->index);
  PutCodeInd(fd,tmp->addr);
}

void WriteBvrTab(int fd, void* entry)
{
  LK_VECTOR_Write(fd,(struct Vector*)entry,&WriteBvrTabEnt);
  if(LK_VECTOR_Size((struct Vector*)entry)!=0)
    LK_VECTOR_Free((struct Vector*)entry);
}

void WriteBvrTabs(int fd)
{
  LK_VECTOR_Write(fd, &BvrTabs,&WriteBvrTab);
  LK_VECTOR_Free(&BvrTabs);
}

int BvrTabSearch(struct Vector* BvrTab,Byte index)
{
  BvrTabEnt* tmp=LK_VECTOR_GetPtr(BvrTab,0);
  int i;
  int size=LK_VECTOR_Size(BvrTab);
  for(i=0;i<size;i++)
  {
    if(tmp[i].index==index)
      return i;
  }
  return -1;
}

void MergeBvrTabs(BvrTabInd a, BvrTabInd b,Byte n)
{
  struct Vector* pa=(struct Vector*)LK_VECTOR_GetPtr(&BvrTabs,a);
  struct Vector* pb=(struct Vector*)LK_VECTOR_GetPtr(&BvrTabs,b);
  
  int size=LK_VECTOR_Size(pb);
  int i,j;
  BvrTabEnt* tmpa=(BvrTabEnt*)LK_VECTOR_GetPtr(pa,0);
  BvrTabEnt* tmpb=(BvrTabEnt*)LK_VECTOR_GetPtr(pb,0);
  for(i=0;i<size;i++)
  {
    j=BvrTabSearch(pa,tmpb[i].index);
    if(j!=-1)
    {
      tmpa[j].addr=MergeSubSequence(tmpa[j].addr,tmpb[i].addr,n);
    }
    else
    {
      BVR_AddEntry(pa,tmpb+i);
      tmpa=(BvrTabEnt*)LK_VECTOR_GetPtr(pa,0);
    }
  }
  LK_VECTOR_Free((struct Vector*)pb);
}

void BVR_AddEntry(struct Vector* BvrTab, BvrTabEnt* entry)
{
  BvrTabEnt* new=LK_VECTOR_GetPtr(BvrTab,LK_VECTOR_Grow(BvrTab,1));
  new->index=entry->index;
  new->addr=entry->addr;
}
