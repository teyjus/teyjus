#include <stdio.h>
#include "vector.h"
#include "datatypes.h"
#include "module.h"
#include "file.h"
#include "hashtab.h"
#include "code.h"
//////////////////////////////////////////////////////
//HashTab Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
  ConstInd index;
  CodeInd addr;
}HashTabEnt;

struct Vector HashTabs;

void HASH_AddEntry(HashTab_t* HashTab, HashTabEnt* entry);

void InitTHashTabs()
{
  InitVec(&HashTabs,32,sizeof(HashTab_t));
}

void LoadHashTabs()
{
  int i;
  TwoBytes count=CM->HashTabcount=GET2();
  int offset=CM->HashTaboffset=Extend(&HashTabs,(int)count);
  HashTab_t* tmp=(HashTab_t*)Fetch(&HashTabs,offset);
  for(i=0;i<count;i++)
  {
    LoadHashTab(tmp+i);
  }
}

void LoadHashTab(HashTab_t* HashTab)
{
  int j;
  TwoBytes count=GET2();
  InitVec((struct Vector*)HashTab,(int)count,sizeof(HashTabEnt));
  Extend((struct Vector*)HashTab,(int)count);
  HashTabEnt* tmp=Fetch((struct Vector*)HashTab,0);
  for(j=0;j<count;j++)
  {
    tmp[j].index=GetConstInd();
    tmp[j].addr=GetCodeInd();
  }
}

void WriteHashTabs()
{
  int i;
  PUT2(HashTabs.numEntries);
  HashTab_t* tmp=(HashTab_t*)Fetch(&HashTabs,0);
  for(i=0;i<HashTabs.numEntries;i++)
  {
    WriteHashTab(tmp+i);
  }
}

void WriteHashTab(HashTab_t* HashTab)
{
  int i;
  TwoBytes count=((struct Vector*)HashTab)->numEntries;
  PUT2(count);
  HashTabEnt* tmp=Fetch((struct Vector*)HashTab,0);
  for(i=0;i<count;i++)
  {
    PutConstInd(tmp[i].index);
    PutCodeInd(tmp[i].addr);
  }
}

int HashTabSearch(HashTab_t* HashTab, ConstInd x)
{
  HashTabEnt* tmp=Fetch((struct Vector*)HashTab,0);
  int i;
  int size=((struct Vector*)HashTab)->numEntries;
  for(i=0;i<size;i++)
  {
    if((tmp[i].index.gl_flag==x.gl_flag)&&(tmp[i].index.index==x.index))
      return i;
  }
  return -1;
}

Byte MergeHashTabs(HashTabInd a, HashTabInd b,Byte n)
{
  HashTab_t* pa=(HashTab_t*)Fetch(&HashTabs,a);
  HashTab_t* pb=(HashTab_t*)Fetch(&HashTabs,b);
  
  int size=((struct Vector*)pb)->numEntries;
  int i,j;
  HashTabEnt* tmpa=(HashTabEnt*)Fetch((struct Vector*)pa,0);
  HashTabEnt* tmpb=(HashTabEnt*)Fetch((struct Vector*)pb,0);
  for(i=0;i<size;i++)
  {
    j=HashTabSearch(pa,tmpb[i].index);
    if(j!=-1)
    {
      tmpa[j].addr=MergeSubSequence(tmpa[j].addr,tmpb[i].addr,n);
    }
    else
    {
      HASH_AddEntry(pa,tmpb+i);
      tmpa=(HashTabEnt*)Fetch((struct Vector*)pa,0);
    }
  }
  Destroy((struct Vector*)pb);
  return ((struct Vector*)pa)->numEntries;
}

void MergeFindCodeTabs(HashTab_t* pa, HashTab_t* pb)
{
  int size=((struct Vector*)pb)->numEntries;
  int i,j;
  HashTabEnt* tmpa=(HashTabEnt*)Fetch((struct Vector*)pa,0);
  HashTabEnt* tmpb=(HashTabEnt*)Fetch((struct Vector*)pb,0);
  for(i=0;i<size;i++)
  {
    j=HashTabSearch(pa,tmpb[i].index);
    if(j!=-1)
    {
      tmpa[j].addr=MergeDefs(tmpa[j].addr,tmpb[i].addr);
    }
    else
    {
      HASH_AddEntry(pa,tmpb+i);
      tmpa=(HashTabEnt*)Fetch((struct Vector*)pa,0);
    }
  }
  Destroy((struct Vector*)pb);
}

void HASH_AddEntry(HashTab_t* HashTab, HashTabEnt* entry)
{
  HashTabEnt* new=Fetch((struct Vector*)HashTab,Extend((struct Vector*)HashTab,1));
  new->index=entry->index;
  new->addr=entry->addr;
}

CodeInd HashCodeAddr(HashTab_t* HashTab, ConstInd x)
{
  
  HashTabEnt* tmp=Fetch((struct Vector*)HashTab,0);
  int i;
  int size=((struct Vector*)HashTab)->numEntries;
  for(i=0;i<size;i++)
  {
    if((tmp[i].index.gl_flag==x.gl_flag)&&(tmp[i].index.index==x.index))
      return tmp[i].addr;
  }
  return -1;
}
