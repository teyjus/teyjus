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
	INT1 findCodeFun;
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
	InitVec(&ImplGoals,32,sizeof(TImplGoal_t));
}

void LoadImplGoals()
{
	int i;
	int count=CM->ImplGoalcount=GET2();
	int offset=CM->ImplGoaloffset=Extend(&ImplGoals,count);
	TImplGoal_t* tmp=(TImplGoal_t*)Fetch(&ImplGoals,offset);
	for(i=0;i<count;i++)
	{
		LoadImplGoal(tmp+i);
	}
}

void LoadImplGoal(TImplGoal_t* ImplGoal)
{
	int j;
	INT2 count=GET2();
	struct Vector* vec=&(ImplGoal->extPred);
	InitVec(vec,(int)count,sizeof(ConstInd));
	Extend(vec,(int)count);
	ConstInd* tmp=(ConstInd*)Fetch(vec,0);
	for(j=0;j<count;j++)
	{
		tmp[j]=GetConstInd();
		//FlagDynamicPred(tmp[j]);
	}
	
	ImplGoal->findCodeFun=GET1();
	
	count=GET2();
	vec=&(ImplGoal->findCodeTab);
	InitVec(vec,(int)count,sizeof(HashTabEnt));
	Extend(vec,(int)count);
	HashTabEnt* tmp2=(HashTabEnt*)Fetch(vec,0);
	for(j=0;j<count;j++)
	{
		tmp2[j].index=GetConstInd();
		tmp2[j].addr=GetCodeInd();
	}
}

void WriteImplGoals()
{
	int i;
	PUT2(ImplGoals.numEntries);
	TImplGoal_t* tmp=Fetch(&ImplGoals,0);
	for(i=0;i<ImplGoals.numEntries;i++)
	{
		WriteImplGoal(tmp+i);
	}
}

void WriteImplGoal(TImplGoal_t* ImplGoal)
{
	int j;
	INT2 count=(ImplGoal->extPred).numEntries;
	PUT2(count);
	
	ConstInd* tmp=(ConstInd*)Fetch(&(ImplGoal->extPred),0);
	for(j=0;j<count;j++)
	{
		PutConstInd(tmp[j]);
	}
	
	PUT1(ImplGoal->findCodeFun);
	
	count = (ImplGoal->findCodeTab).numEntries;
	PUT2(count);
	
	HashTabEnt* tmp2=(HashTabEnt*)Fetch(&(ImplGoal->findCodeTab),0);
	for(j=0;j<count;j++)
	{
		PutConstInd(tmp2[j].index);
		PutCodeInd(tmp2[j].addr);
	}
}
