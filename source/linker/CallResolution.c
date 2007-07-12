#include <stdio.h>
#include <stdlib.h>
#include "datatypes.h"
#include "vector.h"
#include "file.h"
#include "module.h"
#include "hashtab.h"
#include "code.h"
#include "CallResolution.h"

typedef struct{
  ConstInd index;
  int dynamic_flag;
  struct Vector predCalls;
}PredInfo;

typedef struct{
  int arity;
  CodeInd addr;
}PCallEnt;

void InitInfoTab(PredInfoTab* Pit)
{
  LK_VECTOR_Init(Pit,16,sizeof(PredInfo));
}

PredInfo* FindPredInfo(PredInfoTab* Pit, ConstInd index)
{
  int i;
  PredInfo* tmp=NULL;
  int size=LK_VECTOR_Size(Pit);
  if(size>0)
  {
    tmp=LK_VECTOR_GetPtr(Pit,0);
    for(i=0;i<size;i++)
    {
      if((tmp[i].index.gl_flag==index.gl_flag)&&(tmp[i].index.index==index.index))
        return tmp+i;
    }
  }
  tmp=LK_VECTOR_GetPtr(Pit,LK_VECTOR_Grow(Pit,1));
  tmp->index=index;
  tmp->dynamic_flag=0;
  LK_VECTOR_Init(&(tmp->predCalls),8,sizeof(PCallEnt));
  return tmp;
}

void PushCall(PredInfoTab* Pit, ConstInd index,CodeInd addr,int arity)
{
  PredInfo* PInfo = FindPredInfo(Pit,index);
  PCallEnt* tmp = (PCallEnt*)LK_VECTOR_GetPtr(&(PInfo->predCalls),LK_VECTOR_Grow(&(PInfo->predCalls),1));
  tmp->addr=addr;
  tmp->arity=arity;
}

void MarkDynamic(PredInfoTab* Pit, ConstInd index)
{
  PredInfo* PInfo = FindPredInfo(Pit,index);
  PInfo->dynamic_flag++;
}

void ResolvePredCall(PredInfo* PInfo,HashTab_t* PredSearchTab)
{
  int i;
  int size=LK_VECTOR_Size(&(PInfo->predCalls));
  if(size==0)
  {
    LK_VECTOR_Free(&(PInfo->predCalls));
    return;
  }
  PCallEnt* tmp=LK_VECTOR_GetPtr(&(PInfo->predCalls),0);
  CodeInd addr;
  if(PInfo->dynamic_flag>0)
  {
    for(i=0;i<size;i++)
    {
      MakeCallName(tmp[i].addr,tmp[i].arity,PInfo->index);
    }
  }
  else
  {
    addr=HashCodeAddr(PredSearchTab,PInfo->index);
    for(i=0;i<size;i++)
    {
      MakeCall(tmp[i].addr,tmp[i].arity,addr);
    }
  }
  LK_VECTOR_Free(&(PInfo->predCalls));
}

void ResolvePredCalls(PredInfoTab* Pit,HashTab_t* PredSearchTab)
{
  int i;
  int size=LK_VECTOR_Size(Pit);
  PredInfo* tmp=(PredInfo*)LK_VECTOR_GetPtr(Pit,0);
  for(i=0;i<size;i++)
  {
    ResolvePredCall(tmp+i,PredSearchTab);
  }
  LK_VECTOR_Free(Pit);
}

