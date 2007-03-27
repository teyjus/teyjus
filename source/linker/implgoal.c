#include <stdio.h>
#include "vector.h"
#include "file.h"
#include "module.h"
#include "datatypes.h"
//////////////////////////////////////////////////////
//ImplGoal Load and Write Code
//////////////////////////////////////////////////////
typedef struct{
  struct Vector extPred;
  Byte findCodeFun;
  struct Vector findCodeTab;
}TImplGoal_t;

typedef struct{
  ConstInd index;
  CodeInd addr;
}HashTabEnt;

struct Vector ImplGoals;

void LoadImplGoal(TImplGoal_t* ImplGoal);
void WriteImplGoal(TImplGoal_t* ImplGoal);

void InitTImplGoals()
{
  LK_VECTOR_Init(&ImplGoals,32,sizeof(TImplGoal_t));
}

void LoadImplGoals()
{
  int i;
  int count=CM->ImplGoalcount=GET2();
  int offset=CM->ImplGoaloffset=LK_VECTOR_Grow(&ImplGoals,count);
  TImplGoal_t* tmp=(TImplGoal_t*)LK_VECTOR_GetPtr(&ImplGoals,offset);
  for(i=0;i<count;i++)
  {
    LoadImplGoal(tmp+i);
  }
}

void LoadImplGoal(TImplGoal_t* ImplGoal)
{
  int j;
  TwoBytes count=GET2();
  struct Vector* vec=&(ImplGoal->extPred);
  LK_VECTOR_Init(vec,(int)count,sizeof(ConstInd));
  LK_VECTOR_Grow(vec,(int)count);
  ConstInd* tmp=(ConstInd*)LK_VECTOR_GetPtr(vec,0);
  for(j=0;j<count;j++)
  {
    tmp[j]=GetConstInd(PeekInput(),CM);
    //FlagDynamicPred(tmp[j]);
  }
  
  ImplGoal->findCodeFun=GET1();
  
  count=GET2();
  vec=&(ImplGoal->findCodeTab);
  LK_VECTOR_Init(vec,(int)count,sizeof(HashTabEnt));
  LK_VECTOR_Grow(vec,(int)count);
  HashTabEnt* tmp2=(HashTabEnt*)LK_VECTOR_GetPtr(vec,0);
  for(j=0;j<count;j++)
  {
    tmp2[j].index=GetConstInd(PeekInput(),CM);
    tmp2[j].addr=GetCodeInd();
  }
}

void WriteImplGoals()
{
  int i;
  int size=LK_VECTOR_Size(&ImplGoals);
  PUT2(size);
  TImplGoal_t* tmp=LK_VECTOR_GetPtr(&ImplGoals,0);
  for(i=0;i<size;i++)
  {
    WriteImplGoal(tmp+i);
  }
}

void WriteImplGoal(TImplGoal_t* ImplGoal)
{
  int j;
  TwoBytes count=LK_VECTOR_Size(&(ImplGoal->extPred));
  PUT2(count);
  
  ConstInd* tmp=(ConstInd*)LK_VECTOR_GetPtr(&(ImplGoal->extPred),0);
  for(j=0;j<count;j++)
  {
    PutConstInd(PeekOutput(),tmp[j]);
  }
  
  PUT1(ImplGoal->findCodeFun);
  
  count = LK_VECTOR_Size(&(ImplGoal->findCodeTab));
  PUT2(count);
  
  HashTabEnt* tmp2=(HashTabEnt*)LK_VECTOR_GetPtr(&(ImplGoal->findCodeTab),0);
  for(j=0;j<count;j++)
  {
    PutConstInd(PeekOutput(),tmp2[j].index);
    PutCodeInd(PeekOutput(),tmp2[j].addr);
  }
}
