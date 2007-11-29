#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "vector.h"
#include "datatypes.h"
#include "module.h"
#include "file.h"
#include "hashtab.h"
#include "code.h"
#include "VectorRW.h"
#include "message.h"
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
  LK_VECTOR_Init(&HashTabs,32,sizeof(HashTab_t));
}

void LoadHashTabEnt(int fd, struct Module_st* CMData,void* entry)
{
  HashTabEnt* tmp = (HashTabEnt*)entry;
  tmp->index=GetConstInd(fd,CMData);
  tmp->addr=GetCodeInd(fd,CMData);
  debug("HashTabEnt[%d,%d]=%lx\n",tmp->index.gl_flag,tmp->index.index,tmp->addr);
}

void LoadHashTab(int fd, struct Module_st* CMData,void* entry)
{
  debug("Loading HashTab at %lx\n",lseek(fd,0,SEEK_CUR));
  Adjust_t ignore_me;
  LK_VECTOR_Init((struct Vector*)entry,0,sizeof(HashTabEnt));
  LK_VECTOR_Read(fd,(struct Vector*)entry,CMData,&ignore_me,LoadHashTabEnt);
}

void LoadHashTabs(int fd, struct Module_st* CMData)
{
  LK_VECTOR_Read(fd,&HashTabs,CMData,&(CMData->HashTabAdj),LoadHashTab);
}

void WriteHashTabEnt(int fd,void* entry)
{
  HashTabEnt* tmp = (HashTabEnt*)entry;
  PutConstInd(fd,tmp->index);
  PutCodeInd(fd,tmp->addr);
  debug("Writing HashTabEnt[%d,%d]=%lx\n",tmp->index.gl_flag,tmp->index.index,tmp->addr);
}

void WriteHashTab(int fd, void* entry)
{
  debug("Writing HashTab at %lx\n",lseek(fd,0,SEEK_CUR));
  LK_VECTOR_Write(fd,(struct Vector*)entry,&WriteHashTabEnt);
  if(LK_VECTOR_Size((struct Vector*)entry)!=0)
    LK_VECTOR_Free((struct Vector*)entry);
}

void WriteHashTabs(int fd)
{
  LK_VECTOR_Write(fd, &HashTabs,&WriteHashTab);
  LK_VECTOR_Free(&HashTabs);
}

int HashTabSearch(HashTab_t* HashTab, ConstInd x)
{
  HashTabEnt* tmp=LK_VECTOR_GetPtr((struct Vector*)HashTab,0);
  int i;
  int size=LK_VECTOR_Size((struct Vector*)HashTab);
  for(i=0;i<size;i++)
  {
    if((tmp[i].index.gl_flag==x.gl_flag)&&(tmp[i].index.index==x.index))
      return i;
  }
  return -1;
}

Byte MergeHashTabs(HashTabInd a, HashTabInd b,Byte n)
{
  debug("Merging HashTabs %d and %d.\n",a,b);
  HashTab_t* pa=(HashTab_t*)LK_VECTOR_GetPtr(&HashTabs,a);
  HashTab_t* pb=(HashTab_t*)LK_VECTOR_GetPtr(&HashTabs,b);
  
  int size=LK_VECTOR_Size((struct Vector*)pb);
  int i,j;
  HashTabEnt* tmpa=(HashTabEnt*)LK_VECTOR_GetPtr((struct Vector*)pa,0);
  HashTabEnt* tmpb=(HashTabEnt*)LK_VECTOR_GetPtr((struct Vector*)pb,0);
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
      tmpa=(HashTabEnt*)LK_VECTOR_GetPtr((struct Vector*)pa,0);
    }
  }
  LK_VECTOR_Free((struct Vector*)pb);
  return LK_VECTOR_Size((struct Vector*)pa);
}

void MergeFindCodeTabs(HashTab_t* pa, HashTab_t* pb,ConstInd LowLConst)
{
  int size=LK_VECTOR_Size((struct Vector*)pb);
  int i,j;
  HashTabEnt* tmpa=(HashTabEnt*)LK_VECTOR_GetPtr((struct Vector*)pa,0);
  HashTabEnt* tmpb=(HashTabEnt*)LK_VECTOR_GetPtr((struct Vector*)pb,0);
  for(i=0;i<size;i++)
  {
    j=HashTabSearch(pa,tmpb[i].index);
    if(j!=-1)
    {
      tmpa[j].addr=MergeDefs(tmpa[j].addr,tmpb[i].addr);
    }
    else
    {
      if(tmpb[i].index.gl_flag==LOCAL && 
         tmpb[i].index.index>=LowLConst.index)
      {
        debug("[%d:%d] > [%d:%d]\n",
              tmpb[i].index.gl_flag,
              tmpb[i].index.index,
              LowLConst.gl_flag,
              LowLConst.index);
        TidySwitchOnReg(&(tmpb[i].addr));
      }
      HASH_AddEntry(pa,tmpb+i);
      tmpa=(HashTabEnt*)LK_VECTOR_GetPtr((struct Vector*)pa,0);
    }
  }
  LK_VECTOR_Free((struct Vector*)pb);
}

void HASH_AddEntry(HashTab_t* HashTab, HashTabEnt* entry)
{
  HashTabEnt* new=LK_VECTOR_GetPtr((struct Vector*)HashTab,LK_VECTOR_Grow((struct Vector*)HashTab,1));
  new->index=entry->index;
  new->addr=entry->addr;
}

CodeInd HashCodeAddr(HashTab_t* HashTab, ConstInd x)
{
  
  HashTabEnt* tmp=LK_VECTOR_GetPtr((struct Vector*)HashTab,0);
  int i;
  int size=LK_VECTOR_Size((struct Vector*)HashTab);
  for(i=0;i<size;i++)
  {
    if((tmp[i].index.gl_flag==x.gl_flag)&&(tmp[i].index.index==x.index))
      return tmp[i].addr;
  }
  return -1;
}
