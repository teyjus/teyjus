#include "vector.h"
#include "file.h"
#include "datatypes.h"
#include "module.h"
#include "code.h"
//////////////////////////////////////////////////////
//BvrTab Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
  Byte index;
  CodeInd addr;
}BvrTabEnt;

struct Vector BvrTabs;

void BVR_AddEntry(struct Vector* BvrTab, BvrTabEnt* entry);
void LoadBvrTab(struct Vector* BvrTab);
void WriteBvrTab(struct Vector* BvrTab);

void InitTBvrTabs()
{
  LK_VECTOR_Init(&BvrTabs,32,sizeof(struct Vector));
}

void LoadBvrTabs()
{
  int i;
  TwoBytes count=CM->BvrTabcount=GET2();
  int offset=CM->BvrTaboffset=LK_VECTOR_Grow(&BvrTabs,(int)count);
  struct Vector* tmp=(struct Vector*)LK_VECTOR_GetPtr(&BvrTabs,offset);
  for(i=0;i<count;i++)
  {
    LoadBvrTab(tmp+i);
  }
}

void LoadBvrTab(struct Vector* BvrTab)
{
  int j;
  Byte count=GET1();
  LK_VECTOR_Init(BvrTab,(int)count,sizeof(BvrTabEnt));
  LK_VECTOR_Grow(BvrTab,(int)count);
  BvrTabEnt* tmp=(BvrTabEnt*)LK_VECTOR_GetPtr(BvrTab,0);
  for(j=0;j<count;j++)
  {
    tmp[j].index=GET1();
    tmp[j].addr=GetCodeInd();
  }
  
}

void WriteBvrTabs()
{
  int i;
  int size=LK_VECTOR_Size(&BvrTabs);
  PUT2(size);
  struct Vector* tmp=LK_VECTOR_GetPtr(&BvrTabs,0);
  for(i=0;i<size;i++)
  {
    WriteBvrTab(tmp+i);
  }
}

void WriteBvrTab(struct Vector* BvrTab)
{
  int j;
  Byte count=LK_VECTOR_Size(BvrTab);
  PUT1(count);
  BvrTabEnt* tmp=(BvrTabEnt*)LK_VECTOR_GetPtr(BvrTab,0);
  for(j=0;j<count;j++)
  {
    PUT1(tmp[j].index);
    PutCodeInd(tmp[j].addr);
  }
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
}

void BVR_AddEntry(struct Vector* BvrTab, BvrTabEnt* entry)
{
  BvrTabEnt* new=LK_VECTOR_GetPtr(BvrTab,LK_VECTOR_Grow(BvrTab,1));
  new->index=entry->index;
  new->addr=entry->addr;
}
